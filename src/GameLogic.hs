module GameLogic
  ( canMoveTo
  , moveTankPure
  , applyClientCommand
  , stepWorld
  , screenForWorld
  , playerSpeed
  , fireCooldownSeconds
  , shieldDurationSeconds
  , aimAngleTo
  , gameMapFor
  , initialItemsFor
  , spawnPositions
  , livingPlayers
  , matchResultFor
  ) where

import Data.List (find, partition, sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Types

tileSize :: Float
tileSize = 50.0

playerSpeed :: Float
playerSpeed = 180.0

fireCooldownSeconds :: Float
fireCooldownSeconds = 0.25

shieldDurationSeconds :: Float
shieldDurationSeconds = 3.0

defaultBulletSpeed :: Float
defaultBulletSpeed = 420.0

defaultBulletDamage :: Int
defaultBulletDamage = 20

defaultBombFuseSeconds :: Float
defaultBombFuseSeconds = 0.75

defaultBombRadius :: Float
defaultBombRadius = 95.0

defaultBombDamage :: Int
defaultBombDamage = 45

tankHitRadius :: Float
tankHitRadius = 22.0

itemPickupRadius :: Float
itemPickupRadius = 28.0

-- | A missing grid coordinate is outside the map and is therefore blocked.
canMoveTo :: GridPos -> GameMap -> Bool
canMoveTo targetPosition gameMap = case Map.lookup targetPosition gameMap of
  Nothing -> False
  Just info -> tileCollision info == PassThrough

posToGrid :: Pos -> GridPos
posToGrid (x, y) = (round (x / tileSize), round (y / tileSize))

gridToPos :: GridPos -> Pos
gridToPos (gridX, gridY) =
  ( fromIntegral gridX * tileSize
  , fromIntegral gridY * tileSize
  )

-- | Move one alive tank by a world-space distance after checking the map.
-- | Body direction and body rotation change together; turret rotation does not.
moveTankPure :: Float -> Direction -> Player -> GameMap -> Player
moveTankPure distance direction player gameMap
  | playerStatus player /= PlayerAlive = player
  | playerHealth player <= 0 = player { playerStatus = PlayerDead }
  | canMoveTo targetGrid gameMap = player
      { playerPosition = targetPosition
      , playerBodyDirection = direction
      , playerBodyAngle = bodyAngleFor direction
      }
  | otherwise = player
      { playerBodyDirection = direction
      , playerBodyAngle = bodyAngleFor direction
      }
  where
    (positionX, positionY) = playerPosition player
    (deltaX, deltaY) = directionVector direction
    targetPosition =
      ( positionX + deltaX * distance
      , positionY + deltaY * distance
      )
    targetGrid = posToGrid targetPosition

-- | Server-side command processing. Clients only submit intent; all mutable
-- | game state is computed here and then broadcast as a snapshot.
applyClientCommand :: PlayerId -> ClientCommand -> GameWorld -> GameWorld
applyClientCommand sender command world
  | worldStatus world /= MatchRunning = world
  | otherwise = case command of
      CmdInput inputState -> applyInputState sender inputState world
      CmdCreateRoom _ _ -> world
      CmdJoinRoom _ _ -> world
      CmdLeaveRoom -> world
      CmdSelectTank _ _ -> world
      CmdSetReady _ -> world
      CmdSelectMap _ -> world
      CmdStartMatch -> world
      CmdPlayAgain -> world

applyInputState :: PlayerId -> InputState -> GameWorld -> GameWorld
applyInputState sender inputState world = case Map.lookup sender (worldPlayers world) of
  Nothing -> world
  Just originalPlayer ->
    let aimedPlayer = applyAim inputState originalPlayer
        movedPlayer = applyMovement inputState aimedPlayer
        worldWithPlayer = world
          { worldPlayers = Map.insert sender movedPlayer (worldPlayers world) }
        worldAfterItems = collectItems sender worldWithPlayer
    in if canUseBomb inputState movedPlayer
         then placeBomb sender worldAfterItems
         else if canFire inputState movedPlayer
           then spawnBullet sender worldAfterItems
           else worldAfterItems
  where
    applyAim input player = case inputAimAngle input of
      Nothing -> player
      Just angle -> player { playerTurretAngle = normalizeAngle angle }

    applyMovement input player = case inputMoveDirection input of
      Nothing -> player
      Just direction -> moveTankPure playerSpeed direction player (worldMap world)

    canFire input player =
      inputShootPressed input
        && playerStatus player == PlayerAlive
        && playerHealth player > 0
        && playerFireCooldown player <= 0

    canUseBomb input player =
      inputUseBombPressed input
        && playerStatus player == PlayerAlive
        && playerHealth player > 0
        && playerBombCount player > 0

collectItems :: PlayerId -> GameWorld -> GameWorld
collectItems collectorId world = case Map.lookup collectorId (worldPlayers world) of
  Nothing -> world
  Just collector
    | playerStatus collector /= PlayerAlive -> world
    | otherwise -> world
        { worldPlayers = Map.insert collectorId updatedCollector (worldPlayers world)
        , worldItems = remainingItems
        }
    where
      (collectedItems, remainingItems) = partition isCollected (worldItems world)
      isCollected item =
        squaredDistance (playerPosition collector) (itemPosition item)
          <= itemPickupRadius * itemPickupRadius
      updatedCollector = foldl applyItem collector collectedItems

applyItem :: Player -> Item -> Player
applyItem player item = case itemType item of
  ItemHeart -> player
    { playerHealth = min (playerMaxHealth player) (playerHealth player + 30) }
  ItemShield -> player { playerShieldRemaining = shieldDurationSeconds }
  ItemBomb -> player { playerBombCount = playerBombCount player + 1 }

spawnBullet :: PlayerId -> GameWorld -> GameWorld
spawnBullet owner world = case Map.lookup owner (worldPlayers world) of
  Nothing -> world
  Just player -> world
    { worldPlayers = Map.insert owner playerAfterShot (worldPlayers world)
    , worldBullets = newBullet : worldBullets world
    }
    where
      (tankX, tankY) = playerPosition player
      (forwardX, forwardY) = angleVector (playerTurretAngle player)
      muzzlePosition =
        ( tankX + forwardX * turretBarrelLengthFor (playerTankType player)
        , tankY + forwardY * turretBarrelLengthFor (playerTankType player)
        )
      newBullet = Bullet
        { bulletId = nextBulletId world
        , bulletPosition = muzzlePosition
        , bulletAngle = playerTurretAngle player
        , bulletSpeed = defaultBulletSpeed
        , bulletDamage = defaultBulletDamage
        , bulletOwnerId = owner
        }
      playerAfterShot = player { playerFireCooldown = fireCooldownSeconds }

placeBomb :: PlayerId -> GameWorld -> GameWorld
placeBomb owner world = case Map.lookup owner (worldPlayers world) of
  Nothing -> world
  Just player -> world
    { worldPlayers = Map.insert owner playerAfterUse (worldPlayers world)
    , worldBombs = Bomb
        { bombId = nextBombId world
        , bombOwnerId = owner
        , bombPosition = playerPosition player
        , bombFuseRemaining = defaultBombFuseSeconds
        , bombRadius = defaultBombRadius
        , bombDamage = defaultBombDamage
        }
      : worldBombs world
    }
    where
      playerAfterUse = player { playerBombCount = playerBombCount player - 1 }

nextBulletId :: GameWorld -> Int
nextBulletId world = nextIdentifier $ map bulletId (worldBullets world)

nextBombId :: GameWorld -> Int
nextBombId world = nextIdentifier $ map bombId (worldBombs world)

nextIdentifier :: [Int] -> Int
nextIdentifier identifiers = case identifiers of
  [] -> 1
  _ -> 1 + maximum identifiers

-- | These distances match the scaled artwork in 'Render.Core' so a bullet
-- | appears at the end of each real gun barrel.
turretBarrelLengthFor :: TankType -> Float
turretBarrelLengthFor tankType = case tankType of
  TankScout -> 25.0
  TankHeavy -> 24.0
  TankArtillery -> 30.0

-- | Advance server-owned transient objects. The server calls this on its fixed
-- | tick; clients only render the resulting world snapshot.
stepWorld :: Float -> GameWorld -> GameWorld
stepWorld elapsed world = resolveBombs explodedBombs worldAfterBullets
  where
    duration = max 0 elapsed
    worldWithTimers = world
      { worldPlayers = Map.map (advancePlayerTimers duration) (worldPlayers world)
      , worldBullets = []
      , worldBombs = activeBombs
      , worldElapsedSeconds = worldElapsedSeconds world + duration
      }
    advancedBullets = map (advanceBullet duration) (worldBullets world)
    worldAfterBullets = foldl resolveBullet worldWithTimers advancedBullets
    advancedBombs = map (advanceBomb duration) (worldBombs world)
    (explodedBombs, activeBombs) = partition ((<= 0) . bombFuseRemaining) advancedBombs

    resolveBullet updatedWorld bullet
      | not $ canMoveTo (posToGrid $ bulletPosition bullet) (worldMap updatedWorld) = updatedWorld
      | otherwise = case hitPlayer bullet updatedWorld of
          Nothing -> updatedWorld
            { worldBullets = bullet : worldBullets updatedWorld }
          Just target -> damagePlayer (bulletOwnerId bullet) (bulletDamage bullet) target updatedWorld

advancePlayerTimers :: Float -> Player -> Player
advancePlayerTimers duration player = player
  { playerShieldRemaining = max 0 $ playerShieldRemaining player - duration
  , playerFireCooldown = max 0 $ playerFireCooldown player - duration
  }

advanceBullet :: Float -> Bullet -> Bullet
advanceBullet duration bullet = bullet
  { bulletPosition =
      ( positionX + directionX * bulletSpeed bullet * duration
      , positionY + directionY * bulletSpeed bullet * duration
      )
  }
  where
    (positionX, positionY) = bulletPosition bullet
    (directionX, directionY) = angleVector (bulletAngle bullet)

advanceBomb :: Float -> Bomb -> Bomb
advanceBomb duration bomb = bomb
  { bombFuseRemaining = bombFuseRemaining bomb - duration }

hitPlayer :: Bullet -> GameWorld -> Maybe Player
hitPlayer bullet world = find isHitCandidate $ Map.elems (worldPlayers world)
  where
    isHitCandidate player =
      playerId player /= bulletOwnerId bullet
        && playerStatus player == PlayerAlive
        && squaredDistance (bulletPosition bullet) (playerPosition player) <= tankHitRadius * tankHitRadius

resolveBombs :: [Bomb] -> GameWorld -> GameWorld
resolveBombs bombs world = foldl explodeBomb world bombs
  where
    explodeBomb world bomb = foldl damageTarget world affectedPlayers
      where
        affectedPlayers = filter inRadius $ Map.elems (worldPlayers world)
        inRadius player =
          playerStatus player == PlayerAlive
            && squaredDistance (bombPosition bomb) (playerPosition player)
                <= bombRadius bomb * bombRadius bomb
        damageTarget currentWorld target =
          damagePlayer (bombOwnerId bomb) (bombDamage bomb) target currentWorld

damagePlayer :: PlayerId -> Int -> Player -> GameWorld -> GameWorld
damagePlayer attackerId damage target world
  | playerStatus target /= PlayerAlive = world
  | playerShieldRemaining target > 0 = world
  | otherwise = world { worldPlayers = updatedPlayers }
  where
    healthAfterHit = max 0 $ playerHealth target - damage
    targetWasDestroyed = healthAfterHit <= 0
    targetAfterHit = target
      { playerHealth = healthAfterHit
      , playerStatus = if targetWasDestroyed then PlayerDead else PlayerAlive
      }
    playersAfterHit = Map.insert (playerId target) targetAfterHit (worldPlayers world)
    updatedPlayers
      | targetWasDestroyed && attackerId /= playerId target =
          Map.adjust rewardAttacker attackerId playersAfterHit
      | otherwise = playersAfterHit
    rewardAttacker attacker = attacker
      { playerKills = playerKills attacker + 1
      , playerScore = playerScore attacker + 1
      }

livingPlayers :: GameWorld -> [Player]
livingPlayers world = filter ((== PlayerAlive) . playerStatus) $ Map.elems (worldPlayers world)

matchResultFor :: GameWorld -> MatchResult
matchResultFor world = MatchResult
  { resultWinnerId = playerId <$> find ((== PlayerAlive) . playerStatus) (worldPlayers world)
  , resultRanking = zipWith rankingFor [1 :: Int ..] sortedPlayers
  }
  where
    sortedPlayers = sortBy rankingOrder $ Map.elems (worldPlayers world)
    rankingOrder =
      flip (comparing playerScore)
        <> flip (comparing playerKills)
        <> comparing playerName
    rankingFor place player = RankingEntry
      { rankingPlayerId = playerId player
      , rankingPlayerName = playerName player
      , rankingTankType = playerTankType player
      , rankingTankColor = playerTankColor player
      , rankingKills = playerKills player
      , rankingScore = playerScore player
      , rankingPlace = place
      }

-- | Select the match screen for an active world snapshot. Match results are
-- | delivered independently by the authoritative server.
screenForWorld :: GameWorld -> GameScreen
screenForWorld = ScreenMatch

-- | Three large, bounded maps.  Every spawn and item coordinate is checked
-- | against this map before it is placed into a match.
gameMapFor :: MapType -> GameMap
gameMapFor mapType = Map.fromList
  [ (position, tileFor position)
  | gridX <- [-20 .. 20]
  , gridY <- [-15 .. 15]
  , let position = (gridX, gridY)
  ]
  where
    tileFor position
      | isBorder position = TileInfo TileWall Nothing Solid
      | isSolidFeature position = TileInfo TileObstacle Nothing Solid
      | otherwise = TileInfo TileFloor (decorationAt position) PassThrough

    isBorder (gridX, gridY) = abs gridX == 20 || abs gridY == 15
    isSolidFeature position = case mapType of
      MapDepot -> position `elem` depotObstacles
      MapForest -> position `elem` forestObstacles
      MapRuins -> position `elem` ruinsObstacles
    decorationAt position = case mapType of
      MapDepot
        | position `elem` [(3, 3), (-8, 7)] -> Just PropSign
      MapForest
        | position `elem` [(-6, -6), (7, 5)] -> Just PropPlant
      MapRuins
        | position `elem` [(-4, 8), (9, -3)] -> Just PropRubble
      _ -> Nothing

depotObstacles, forestObstacles, ruinsObstacles :: [GridPos]
depotObstacles =
  [ (gridX, gridY)
  | gridX <- [-3 .. 3]
  , gridY <- [-1 .. 1]
  ]
forestObstacles =
  [ (gridX, gridY)
  | gridX <- [-2 .. 2]
  , gridY <- [-7 .. -5]
  ] ++ [(7, 4), (8, 4), (7, 5), (-9, 5), (-9, 6)]
ruinsObstacles =
  [ (gridX, 4) | gridX <- [-8 .. -3] ]
    ++ [ (gridX, -4) | gridX <- [3 .. 8] ]
    ++ [(-10, -7), (10, 7)]

spawnPositions :: [Pos]
spawnPositions = map gridToPos [(-12, -8), (12, -8), (0, 10)]

-- | These positions are all floor cells on every bundled map.
initialItemsFor :: MapType -> GameMap -> [Item]
initialItemsFor _ gameMap =
  [ Item 1 ItemHeart (validPosition (0, -10))
  , Item 2 ItemShield (validPosition (-12, 5))
  , Item 3 ItemBomb (validPosition (12, 5))
  ]
  where
    validPosition gridPosition
      | canMoveTo gridPosition gameMap = gridToPos gridPosition
      | otherwise = gridToPos $ fromMaybeWalkable (Map.keys gameMap)

    fromMaybeWalkable positions = case find (`canMoveTo` gameMap) positions of
      Just walkablePosition -> walkablePosition
      Nothing -> (0, 0)

aimAngleTo :: Pos -> Pos -> Float
aimAngleTo (originX, originY) (targetX, targetY) =
  normalizeAngle $ radiansToDegrees $ atan2 (originX - targetX) (targetY - originY)

bodyAngleFor :: Direction -> Float
bodyAngleFor direction = case direction of
  DirUp -> 0
  DirRight -> 270
  DirDown -> 180
  DirLeft -> 90

directionVector :: Direction -> Pos
directionVector direction = case direction of
  DirUp -> (0, 1)
  DirDown -> (0, -1)
  DirLeft -> (-1, 0)
  DirRight -> (1, 0)

-- | Angles use the server/bullet convention: zero faces up and 270 faces
-- | right. Render.Core performs the bitmap-only visual correction.
angleVector :: Float -> Pos
angleVector angle =
  ( - sin (degreesToRadians angle)
  , cos (degreesToRadians angle)
  )

squaredDistance :: Pos -> Pos -> Float
squaredDistance (firstX, firstY) (secondX, secondY) =
  deltaX * deltaX + deltaY * deltaY
  where
    deltaX = firstX - secondX
    deltaY = firstY - secondY

normalizeAngle :: Float -> Float
normalizeAngle angle = angle - fromIntegral (floor (angle / 360) :: Int) * 360

degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * pi / 180

radiansToDegrees :: Float -> Float
radiansToDegrees radians = radians * 180 / pi
