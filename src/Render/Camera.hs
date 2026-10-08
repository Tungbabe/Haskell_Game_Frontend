module Render.Camera
  ( virtualWidth
  , virtualHeight
  , Viewport(..)
  , initialViewport
  , viewportScale
  , renderInVirtualCanvas
  , windowToVirtual
  , cameraZoom
  , applyCamera
  , screenToWorld
  , visionClearRadius
  , visionMediumRadius
  , visionOuterRadius
  , isWithinVision
  , renderFogOfWar
  ) where

import Graphics.Gloss
import Types

-- | All UI and camera math use this stable coordinate system.  The actual
-- | window can resize freely; 'renderInVirtualCanvas' letterboxes it while
-- | preserving this aspect ratio.
virtualWidth, virtualHeight :: Float
virtualWidth = 1280
virtualHeight = 720

data Viewport = Viewport
  { viewportPixelWidth :: Int
  , viewportPixelHeight :: Int
  }
  deriving (Show, Eq)

initialViewport :: Viewport
initialViewport = Viewport (round virtualWidth) (round virtualHeight)

viewportScale :: Viewport -> Float
viewportScale viewport
  | viewportPixelWidth viewport <= 0 || viewportPixelHeight viewport <= 0 = 1
  | otherwise = min horizontalScale verticalScale
  where
    horizontalScale = fromIntegral (viewportPixelWidth viewport) / virtualWidth
    verticalScale = fromIntegral (viewportPixelHeight viewport) / virtualHeight

-- | Gloss coordinates are centred in both the real window and virtual canvas,
-- | so scaling alone keeps the letterboxed canvas centred.
renderInVirtualCanvas :: Viewport -> Picture -> Picture
renderInVirtualCanvas viewport = Scale scaleFactor scaleFactor
  where
    scaleFactor = viewportScale viewport

-- | Convert a mouse position reported in the resized real window to the
-- | fixed virtual canvas.  Letterbox margins naturally map outside the canvas.
windowToVirtual :: Viewport -> Pos -> Pos
windowToVirtual viewport (windowX, windowY) =
  (windowX / scaleFactor, windowY / scaleFactor)
  where
    scaleFactor = viewportScale viewport

cameraZoom :: Float
cameraZoom = 1.0

-- | applyCamera translates and scales the entire world picture so that
--   the given target position (e.g., player tank) is at the center of the screen.
applyCamera :: (Float, Float) -> Float -> Picture -> Picture
applyCamera (px, py) zoomLevel worldPicture =
  Scale zoomLevel zoomLevel $ Translate (-px) (-py) worldPicture

-- | Convert a Gloss mouse coordinate, whose origin is the window centre, into
-- | world coordinates for a camera centred on the local tank.
screenToWorld :: Pos -> Float -> Pos -> Pos
screenToWorld (cameraX, cameraY) zoomLevel (screenX, screenY) =
  ( cameraX + screenX / zoomLevel
  , cameraY + screenY / zoomLevel
  )

-- | Visibility tuning is frontend-only.  Keep these values in world pixels so
-- | changing the window size never changes how much of a map is revealed.
visionClearRadius, visionMediumRadius, visionOuterRadius :: Float
visionClearRadius = 180
visionMediumRadius = 320
visionOuterRadius = 500

isWithinVision :: Pos -> Pos -> Bool
isWithinVision origin position = squaredDistance origin position <= visionOuterRadius * visionOuterRadius

-- | Gloss has no shader/stencil mask, so the fog is simulated with three
-- | translucent polygon rings.  Content beyond 'visionOuterRadius' is culled
-- | before rendering, leaving it hidden by the dark window background.
renderFogOfWar :: Pos -> Picture
renderFogOfWar center = Pictures
  [ fogRing center visionClearRadius visionMediumRadius 0.18
  , fogRing center visionMediumRadius 420 0.43
  , fogRing center 420 visionOuterRadius 0.72
  ]

fogRing :: Pos -> Float -> Float -> Float -> Picture
fogRing center innerRadius outerRadius opacity =
  Color (makeColor 0 0 0 opacity) $ Pictures ringSegments
  where
    segmentCount = 56 :: Int
    ringSegments = map ringSegment [0 .. segmentCount - 1]
    ringSegment index = Polygon
      [ pointAt outerRadius index
      , pointAt outerRadius (index + 1)
      , pointAt innerRadius (index + 1)
      , pointAt innerRadius index
      ]
    pointAt radius index =
      ( centerX + cos angle * radius
      , centerY + sin angle * radius
      )
      where
        (centerX, centerY) = center
        angle = 2 * pi * fromIntegral index / fromIntegral segmentCount

squaredDistance :: Pos -> Pos -> Float
squaredDistance (firstX, firstY) (secondX, secondY) =
  deltaX * deltaX + deltaY * deltaY
  where
    deltaX = firstX - secondX
    deltaY = firstY - secondY
