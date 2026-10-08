module Render.Core
  ( renderScreen
  , renderWorld
  , windowWidth
  , windowHeight
  , ExplosionEffect(..)
  , explosionFrameDuration
  , explosionLifetime
  , advanceExplosionEffects
  ) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Render.Camera
  ( applyCamera
  , cameraZoom
  , isWithinVision
  , renderFogOfWar
  , virtualHeight
  , virtualWidth
  )
import Render.Map (renderGameMapInViewport)
import Render.UI
  ( MenuAction
  , renderCreateRoom
  , renderJoinRoom
  , renderLobby
  , renderMainMenu
  , renderResults
  , renderStatusMessage
  , renderUI
  )
import Types

windowWidth, windowHeight :: Int
windowWidth = round virtualWidth
windowHeight = round virtualHeight

-- | A presentation-only effect derived from authoritative PlayerDead snapshots.
data ExplosionEffect = ExplosionEffect
  { explosionPlayerId :: PlayerId
  , explosionPosition :: Pos
  , explosionElapsed :: Float
  }
  deriving (Show, Eq)

explosionFrameDuration :: Float
explosionFrameDuration = 0.08

explosionLifetime :: Float
explosionLifetime = explosionFrameDuration * 8

advanceExplosionEffects :: Float -> [ExplosionEffect] -> [ExplosionEffect]
advanceExplosionEffects elapsed = filter isVisible . map advance
  where
    duration = max 0 elapsed
    advance effect = effect { explosionElapsed = explosionElapsed effect + duration }
    isVisible effect = explosionElapsed effect < explosionLifetime

renderScreen
  :: Assets
  -> PlayerId
  -> String
  -> Maybe MenuAction
  -> Maybe String
  -> [ExplosionEffect]
  -> GameScreen
  -> Picture
renderScreen assets localId joinCode hoverAction statusMessage explosionEffects screen =
  Pictures [screenPicture, renderStatusMessage statusMessage]
  where
    screenPicture = case screen of
      ScreenMainMenu -> renderMainMenu assets hoverAction
      ScreenCreateRoom -> renderCreateRoom assets
      ScreenJoinRoom -> renderJoinRoom assets joinCode
      ScreenLobby room -> renderLobby assets localId room
      ScreenMatch world -> renderWorld assets localId explosionEffects world
      ScreenResults result -> renderResults (Just localId) result

renderWorld :: Assets -> PlayerId -> [ExplosionEffect] -> GameWorld -> Picture
renderWorld assets localId explosionEffects world = Pictures [cameraView, userInterface]
  where
    localTank = Map.lookup localId (worldPlayers world)
    cameraPosition = maybe (0, 0) playerPosition localTank
    worldPicture = Pictures
      [ renderGameMapInViewport cameraPosition cameraZoom (worldMap world)
      , renderTanks assets cameraPosition (worldPlayers world)
      , renderBullets assets cameraPosition (worldPlayers world) (worldBullets world)
      , renderItems cameraPosition (worldItems world)
      , renderBombs cameraPosition (worldBombs world)
      , renderExplosions assets cameraPosition explosionEffects
      , renderFogOfWar cameraPosition
      ]
    cameraView = applyCamera cameraPosition cameraZoom worldPicture
    userInterface = renderUI localTank

renderTanks :: Assets -> Pos -> Map.Map PlayerId Player -> Picture
renderTanks assets cameraPosition tankByPlayer =
  Pictures $ map (renderTank assets) visibleTanks
  where
    visibleTanks = filter isRenderableTank $ Map.elems tankByPlayer
    isRenderableTank player =
      playerStatus player == PlayerAlive
        && isVisible cameraPosition (playerPosition player)

renderTank :: Assets -> Player -> Picture
renderTank assets player =
  Translate positionX positionY $ Pictures
    [ renderTankBody assets player
    , renderTankTurret assets player
    , Translate (-25) 42 $ Scale 0.11 0.11 $ Color white $ Text (playerName player)
    ]
  where
    (positionX, positionY) = playerPosition player

renderTankBody :: Assets -> Player -> Picture
renderTankBody assets player =
  Rotate (playerBodyAngle player) $
    Scale tankSpriteScale tankSpriteScale bodySprite
  where
    spriteKey =
      ( playerTankType player
      , playerTankColor player
      , playerBodyDirection player
      )
    bodySprite = Map.findWithDefault blank spriteKey (assetsTankBodySprites assets)

renderTankTurret :: Assets -> Player -> Picture
renderTankTurret assets player =
  -- Gloss rotates bitmap artwork in the opposite visual direction from the
  -- angle convention used by GameLogic's bullet vectors.  Negating here keeps
  -- simulation/network angles unchanged while the top-facing BMP tracks aim.
  Rotate (- playerTurretAngle player) $
    Scale tankSpriteScale tankSpriteScale turretSprite
  where
    spriteKey = (playerTankType player, playerTankColor player)
    turretSprite = Map.findWithDefault blank spriteKey (assetsTankTurretSprites assets)

-- | The supplied hull artwork is 256 pixels wide; this produces a tank about
-- | 61 world pixels wide and keeps every hull/gun pair aligned.
tankSpriteScale :: Float
tankSpriteScale = 0.24

renderBullets :: Assets -> Pos -> Map.Map PlayerId Player -> [Bullet] -> Picture
renderBullets assets cameraPosition playerById bulletsToRender =
  Pictures $ map renderBullet $ filter (isVisible cameraPosition . bulletPosition) bulletsToRender
  where
    renderBullet bullet =
      Translate positionX positionY $
        Rotate (- bulletAngle bullet) $
          Scale bulletSpriteScale bulletSpriteScale bulletSprite
      where
        (positionX, positionY) = bulletPosition bullet
        ownerColor = maybe TankRed playerTankColor $ Map.lookup (bulletOwnerId bullet) playerById
        bulletSprite = Map.findWithDefault blank ownerColor (assetsBulletSprites assets)

bulletSpriteScale :: Float
bulletSpriteScale = 0.14

renderItems :: Pos -> [Item] -> Picture
renderItems cameraPosition itemsToRender =
  Pictures $ map renderItem $ filter (isVisible cameraPosition . itemPosition) itemsToRender

renderItem :: Item -> Picture
renderItem item =
  Translate positionX positionY $ Color itemColor $ circleSolid 10
  where
    (positionX, positionY) = itemPosition item
    itemColor = case itemType item of
      ItemHeart -> red
      ItemShield -> cyan
      ItemBomb -> orange

renderBombs :: Pos -> [Bomb] -> Picture
renderBombs cameraPosition bombsToRender =
  Pictures $ map renderBomb $ filter (isVisible cameraPosition . bombPosition) bombsToRender

renderBomb :: Bomb -> Picture
renderBomb bomb =
  Translate positionX positionY $ Color orange $ circleSolid 12
  where
    (positionX, positionY) = bombPosition bomb

renderExplosions :: Assets -> Pos -> [ExplosionEffect] -> Picture
renderExplosions assets cameraPosition effects =
  Pictures $ map renderEffect $ filter (isVisible cameraPosition . explosionPosition) effects
  where
    renderEffect effect =
      Translate positionX positionY $
        Scale explosionSpriteScale explosionSpriteScale framePicture
      where
        (positionX, positionY) = explosionPosition effect
        framePicture = explosionFrameAt (explosionElapsed effect) (assetsExplosionFrames assets)

explosionFrameAt :: Float -> [Picture] -> Picture
explosionFrameAt _ [] = blank
explosionFrameAt elapsed frames = frames !! frameIndex
  where
    frameIndex = min (length frames - 1) $ max 0 $ floor (elapsed / explosionFrameDuration)

explosionSpriteScale :: Float
explosionSpriteScale = 0.42

isVisible :: Pos -> Pos -> Bool
isVisible (cameraX, cameraY) (positionX, positionY) =
  isWithinVision (cameraX, cameraY) (positionX, positionY)
    && abs (positionX - cameraX) <= virtualWidth / 2 / cameraZoom + 50
    && abs (positionY - cameraY) <= virtualHeight / 2 / cameraZoom + 50
