module Main where

import qualified Data.Map.Strict as Map
import GameLogic
import NetworkClient (decodeServerResponse)
import Sound (soundFileFor)
import Test.Hspec
import Types

playerOneId, playerTwoId :: PlayerId
playerOneId = PlayerId "p1"
playerTwoId = PlayerId "p2"

floorTile, wallTile :: TileInfo
floorTile = TileInfo TileFloor Nothing PassThrough
wallTile = TileInfo TileWall Nothing Solid

testMap :: GameMap
testMap = Map.fromList
  [ ((gridX, gridY), if (gridX, gridY) == (1, 0) then wallTile else floorTile)
  | gridX <- [-3 .. 3]
  , gridY <- [-3 .. 3]
  ]

basePlayer :: PlayerId -> Pos -> Player
basePlayer playerIdValue position = Player
  { playerId = playerIdValue
  , playerName = show playerIdValue
  , playerTankType = TankScout
  , playerTankColor = TankRed
  , playerPosition = position
  , playerBodyDirection = DirUp
  , playerBodyAngle = 0
  , playerTurretAngle = 0
  , playerHealth = 100
  , playerMaxHealth = 100
  , playerStatus = PlayerAlive
  , playerShieldRemaining = 0
  , playerFireCooldown = 0
  , playerBombCount = 0
  , playerKills = 0
  , playerScore = 0
  }

baseWorld :: GameWorld
baseWorld = GameWorld
  { worldRoomCode = RoomCode "TEST01"
  , worldMapType = MapDepot
  , worldMap = testMap
  , worldPlayers = Map.fromList
      [ (playerOneId, basePlayer playerOneId (0, 0))
      , (playerTwoId, basePlayer playerTwoId (-100, -100))
      ]
  , worldBullets = []
  , worldItems = []
  , worldBombs = []
  , worldElapsedSeconds = 0
  , worldStatus = MatchRunning
  }

main :: IO ()
main = hspec $ do
  describe "GameLogic.canMoveTo" $ do
    it "allows PassThrough tiles" $ do
      canMoveTo (0, 0) testMap `shouldBe` True

    it "blocks Solid and out-of-bounds tiles" $ do
      canMoveTo (1, 0) testMap `shouldBe` False
      canMoveTo (99, 99) testMap `shouldBe` False

  describe "GameLogic.moveTankPure" $ do
    it "updates only the body movement state on a valid move" $ do
      let player = basePlayer playerOneId (0, 0)
          movedPlayer = moveTankPure 10 DirUp player testMap
      playerPosition movedPlayer `shouldBe` (0, 10)
      playerBodyDirection movedPlayer `shouldBe` DirUp
      playerTurretAngle movedPlayer `shouldBe` 0

    it "does not move through a Solid tile" $ do
      let player = basePlayer playerOneId (0, 0)
          movedPlayer = moveTankPure 26 DirRight player testMap
      playerPosition movedPlayer `shouldBe` (0, 0)
      playerBodyDirection movedPlayer `shouldBe` DirRight

  describe "GameLogic.applyClientCommand" $ do
    it "creates a server-authoritative bullet at the turret barrel with cooldown" $ do
      let input = InputState 1 Nothing (Just 270) True False
          updatedWorld = applyClientCommand playerOneId (CmdInput input) baseWorld
          updatedPlayer = worldPlayers updatedWorld Map.! playerOneId
          createdBullet = head $ worldBullets updatedWorld
      bulletAngle createdBullet `shouldBe` 270
      bulletOwnerId createdBullet `shouldBe` playerOneId
      playerFireCooldown updatedPlayer `shouldBe` fireCooldownSeconds
      length (worldBullets updatedWorld) `shouldBe` 1

    it "picks up hearts without exceeding maximum health" $ do
      let injuredPlayer = (basePlayer playerOneId (0, 0)) { playerHealth = 90 }
          worldWithHeart = baseWorld
            { worldPlayers = Map.insert playerOneId injuredPlayer (worldPlayers baseWorld)
            , worldItems = [Item 1 ItemHeart (0, 0)]
            }
          updatedWorld = applyClientCommand playerOneId (CmdInput $ InputState 1 Nothing Nothing False False) worldWithHeart
      playerHealth (worldPlayers updatedWorld Map.! playerOneId) `shouldBe` 100
      worldItems updatedWorld `shouldBe` []

  describe "NetworkClient.decodeServerResponse" $ do
    it "accepts exactly one complete server response" $ do
      decodeServerResponse "ResCommandRejected \"invalid room\""
        `shouldBe` Just (ResCommandRejected "invalid room")

    it "rejects trailing protocol data" $ do
      decodeServerResponse "ResCommandRejected \"bad\" ResConnected (PlayerId \"p1\")"
        `shouldBe` Nothing

  describe "Sound.soundFileFor" $ do
    it "keeps configured shooting audio asynchronous and replaceable" $ do
      soundFileFor SfxShoot `shouldBe` "assets/sounds/shoot.wav"
