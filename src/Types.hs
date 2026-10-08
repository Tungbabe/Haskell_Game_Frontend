module Types
  ( Direction(..)
  , Pos
  , GridPos
  , CollisionType(..)
  , TileType(..)
  , PropType(..)
  , TileInfo(..)
  , GameMap
  , ItemType(..)
  , Item(..)
  , TankType(..)
  , TankColor(..)
  , PlayerId(..)
  , PlayerStatus(..)
  , Player(..)
  , Bullet(..)
  , Bomb(..)
  , MapType(..)
  , RoomCode(..)
  , RoomPlayer(..)
  , RoomStatus(..)
  , GameRoom(..)
  , InputState(..)
  , InterpolationState(..)
  , MatchStatus(..)
  , GameWorld(..)
  , RankingEntry(..)
  , MatchResult(..)
  , GameScreen(..)
  , ClientCommand(..)
  , ServerResponse(..)
  , UiAsset(..)
  , Assets(..)
  , MusicTrack(..)
  , SoundEffect(..)
  , AudioCommand(..)
  ) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss (Picture)

-- | Positions in world-space pixels.  Grid coordinates remain integral so they
-- | are safe keys for 'GameMap'.
type Pos = (Float, Float)

type GridPos = (Int, Int)

-- | The four directions used for movement, shooting, and directional sprites.
data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Show, Read, Eq, Ord)

data CollisionType
  = Solid
  | PassThrough
  deriving (Show, Read, Eq, Ord)

-- | Large map objects.  Every constructor other than 'TileFloor' is intended
-- | to be created with 'Solid' collision.
data TileType
  = TileFloor
  | TileWall
  | TileBox
  | TileFurniture
  | TileObstacle
  deriving (Show, Read, Eq, Ord)

-- | The only three decorative map props.  They are rendered over floor tiles
-- | and always use 'PassThrough' collision.
data PropType
  = PropPlant
  | PropSign
  | PropRubble
  deriving (Show, Read, Eq, Ord)

data TileInfo = TileInfo
  { tileType :: TileType
  , tileDecoration :: Maybe PropType
  , tileCollision :: CollisionType
  }
  deriving (Show, Read, Eq)

type GameMap = Map.Map GridPos TileInfo

data ItemType
  = ItemHeart
  | ItemShield
  | ItemBomb
  deriving (Show, Read, Eq, Ord)

data Item = Item
  { itemId :: Int
  , itemType :: ItemType
  , itemPosition :: Pos
  }
  deriving (Show, Read, Eq)

data TankType
  = TankScout
  | TankHeavy
  | TankArtillery
  deriving (Show, Read, Eq, Ord)

data TankColor
  = TankRed
  | TankBlue
  | TankGreen
  deriving (Show, Read, Eq, Ord)

newtype PlayerId = PlayerId String
  deriving (Show, Read, Eq, Ord)

newtype RoomCode = RoomCode String
  deriving (Show, Read, Eq, Ord)

data PlayerStatus
  = PlayerAlive
  | PlayerDead
  deriving (Show, Read, Eq, Ord)

-- | A player state in an authoritative game snapshot.  The server alone
-- | changes health, shield duration, bombs, score, kills, and status.
data Player = Player
  { playerId :: PlayerId
  , playerName :: String
  , playerTankType :: TankType
  , playerTankColor :: TankColor
  , playerPosition :: Pos
  , playerBodyDirection :: Direction
  , playerBodyAngle :: Float
  , playerTurretAngle :: Float
  , playerHealth :: Int
  , playerMaxHealth :: Int
  , playerStatus :: PlayerStatus
  , playerShieldRemaining :: Float
  , playerFireCooldown :: Float
  , playerBombCount :: Int
  , playerKills :: Int
  , playerScore :: Int
  }
  deriving (Show, Read, Eq)

data Bullet = Bullet
  { bulletId :: Int
  , bulletPosition :: Pos
  , bulletAngle :: Float
  , bulletSpeed :: Float
  , bulletDamage :: Int
  , bulletOwnerId :: PlayerId
  }
  deriving (Show, Read, Eq)

-- | A bomb remains in the world until its fuse reaches zero, at which point
-- | the server applies damage to alive, unshielded players in its radius.
data Bomb = Bomb
  { bombId :: Int
  , bombOwnerId :: PlayerId
  , bombPosition :: Pos
  , bombFuseRemaining :: Float
  , bombRadius :: Float
  , bombDamage :: Int
  }
  deriving (Show, Read, Eq)

data MapType
  = MapDepot
  | MapForest
  | MapRuins
  deriving (Show, Read, Eq, Ord)

data RoomPlayer = RoomPlayer
  { roomPlayerId :: PlayerId
  , roomPlayerName :: String
  , roomPlayerTankType :: Maybe TankType
  , roomPlayerTankColor :: Maybe TankColor
  , roomPlayerReady :: Bool
  }
  deriving (Show, Read, Eq)

data RoomStatus
  = RoomLobby
  | RoomPlaying
  | RoomFinished
  deriving (Show, Read, Eq, Ord)

-- | A room has a server-enforced capacity of two or three players.
data GameRoom = GameRoom
  { roomCode :: RoomCode
  , roomHostId :: PlayerId
  , roomPlayers :: Map.Map PlayerId RoomPlayer
  , roomSelectedMap :: Maybe MapType
  , roomMaxPlayers :: Int
  , roomStatus :: RoomStatus
  }
  deriving (Show, Read, Eq)

-- | Input is sent from the client to the server.  The sequence number lets the
-- | server discard delayed input without trusting a client position.
data InputState = InputState
  { inputSequence :: Int
  , inputMoveDirection :: Maybe Direction
  , inputAimAngle :: Maybe Float
  , inputShootPressed :: Bool
  , inputUseBombPressed :: Bool
  }
  deriving (Show, Read, Eq)

-- | Client-only state used to lerp a remote tank from the last rendered
-- | position towards the latest server position.
data InterpolationState = InterpolationState
  { interpolationPosition :: Pos
  , interpolationTarget :: Pos
  }
  deriving (Show, Read, Eq)

data MatchStatus
  = MatchWaiting
  | MatchRunning
  | MatchComplete
  deriving (Show, Read, Eq, Ord)

-- | A complete server-authoritative match snapshot.  Clients may render it,
-- | but may only predict their own input locally; all gameplay fields are
-- | validated by the server.
data GameWorld = GameWorld
  { worldRoomCode :: RoomCode
  , worldMapType :: MapType
  , worldMap :: GameMap
  , worldPlayers :: Map.Map PlayerId Player
  , worldBullets :: [Bullet]
  , worldItems :: [Item]
  , worldBombs :: [Bomb]
  , worldElapsedSeconds :: Float
  , worldStatus :: MatchStatus
  }
  deriving (Show, Read, Eq)

data RankingEntry = RankingEntry
  { rankingPlayerId :: PlayerId
  , rankingPlayerName :: String
  , rankingTankType :: TankType
  , rankingTankColor :: TankColor
  , rankingKills :: Int
  , rankingScore :: Int
  , rankingPlace :: Int
  }
  deriving (Show, Read, Eq)

data MatchResult = MatchResult
  { resultWinnerId :: Maybe PlayerId
  , resultRanking :: [RankingEntry]
  }
  deriving (Show, Read, Eq)

data GameScreen
  = ScreenMainMenu
  | ScreenCreateRoom
  | ScreenJoinRoom
  | ScreenLobby GameRoom
  | ScreenMatch GameWorld
  | ScreenResults MatchResult
  deriving (Show, Read, Eq)

-- | All values sent from a client to a server.  In particular, no command
-- | carries a requested position, health value, or damage amount.
data ClientCommand
  = CmdCreateRoom String Int
  | CmdJoinRoom RoomCode String
  | CmdLeaveRoom
  | CmdSelectTank TankType TankColor
  | CmdSetReady Bool
  | CmdSelectMap MapType
  | CmdStartMatch
  | CmdInput InputState
  | CmdPlayAgain
  deriving (Show, Read, Eq)

data ServerResponse
  = ResConnected PlayerId
  | ResRoomCreated GameRoom
  | ResRoomUpdated GameRoom
  | ResMatchStarted GameWorld
  | ResWorldSnapshot GameWorld
  | ResMatchEnded MatchResult
  | ResAudio SoundEffect
  | ResCommandRejected String
  deriving (Show, Read, Eq)

-- | UI assets are separate so every screen can use its own BMP image.
data UiAsset
  = UiMenuBackground
  | UiCreateRoomButton
  | UiJoinRoomButton
  | UiStartMatchButton
  | UiPlayAgainButton
  | UiMainMenuButton
  | UiVictoryPanel
  | UiDefeatPanel
  deriving (Show, Read, Eq, Ord)

-- | Loaded image data.  'Picture' intentionally has no 'Read' instance, so
-- | assets stay out of all network messages and do not derive serialization.
data Assets = Assets
  { assetsTankBodySprites :: Map.Map (TankType, TankColor, Direction) Picture
  , assetsTankTurretSprites :: Map.Map (TankType, TankColor) Picture
  , assetsTileSprites :: Map.Map TileType Picture
  , assetsPropSprites :: Map.Map PropType Picture
  , assetsItemSprites :: Map.Map ItemType Picture
  , assetsBulletSprites :: Map.Map TankColor Picture
  , assetsExplosionFrames :: [Picture]
  , assetsMenuBackground :: Picture
  , assetsCreateRoomButton :: Picture
  , assetsJoinRoomButton :: Picture
  , assetsExitButton :: Picture
  , assetsMapButtonSprites :: Map.Map MapType Picture
  , assetsUiSprites :: Map.Map UiAsset Picture
  }

data MusicTrack
  = MusicMenu
  | MusicMatch
  deriving (Show, Read, Eq, Ord)

data SoundEffect
  = SfxUiClick
  | SfxShoot
  | SfxItemPickup ItemType
  | SfxBombExplosion
  | SfxKill
  | SfxDeath
  | SfxVictory
  | SfxDefeat
  deriving (Show, Read, Eq)

-- | Commands consumed by a background audio worker through a TChan.
data AudioCommand
  = PlayMusic MusicTrack
  | StopMusic
  | PlaySound SoundEffect
  deriving (Show, Read, Eq)
