module Render.Map
  ( renderGameMap
  , renderGameMapInViewport
  ) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Render.Camera (isWithinVision, virtualHeight, virtualWidth)
import Types

tileSize :: Float
tileSize = 50.0

-- | Render all base tiles and their optional pass-through decoration.
renderGameMap :: GameMap -> Picture
renderGameMap gameMap = Pictures $ map renderTile (Map.toList gameMap)

-- | Large maps are culled before rendering.  The margin prevents tiles from
-- | popping at the edge while the camera is moving.
renderGameMapInViewport :: Pos -> Float -> GameMap -> Picture
renderGameMapInViewport cameraPosition zoomLevel gameMap =
  Pictures $ map renderTile visibleTiles
  where
    visibleTiles = filter isVisible $ Map.toList gameMap
    isVisible (gridPosition, _) =
      let tilePosition = gridToPos gridPosition
      in withinViewport cameraPosition zoomLevel tilePosition
          && isWithinVision cameraPosition tilePosition

gridToPos :: GridPos -> Pos
gridToPos (gridX, gridY) =
  ( fromIntegral gridX * tileSize
  , fromIntegral gridY * tileSize
  )

withinViewport :: Pos -> Float -> Pos -> Bool
withinViewport (cameraX, cameraY) zoomLevel (positionX, positionY) =
  abs (positionX - cameraX) <= viewportHalfWidth / zoomLevel
    && abs (positionY - cameraY) <= viewportHalfHeight / zoomLevel

viewportHalfWidth, viewportHalfHeight :: Float
viewportHalfWidth = virtualWidth / 2 + tileSize
viewportHalfHeight = virtualHeight / 2 + tileSize

renderTile :: (GridPos, TileInfo) -> Picture
renderTile ((gridX, gridY), info) =
  Translate x y $ Pictures
    [ renderBaseTile (tileType info)
    , renderDecoration (tileDecoration info)
    ]
  where
    x = fromIntegral gridX * tileSize
    y = fromIntegral gridY * tileSize

renderBaseTile :: TileType -> Picture
renderBaseTile tile = case tile of
  TileFloor -> Color (greyN 0.20) $ rectangleSolid tileSize tileSize
  TileWall -> Color (greyN 0.55) $ rectangleSolid tileSize tileSize
  TileBox -> Color (makeColorI 130 82 45 255) $ rectangleSolid tileSize tileSize
  TileFurniture -> Color (makeColorI 92 58 38 255) $ rectangleSolid tileSize tileSize
  TileObstacle -> Color (makeColorI 105 105 105 255) $ circleSolid (tileSize * 0.42)

renderDecoration :: Maybe PropType -> Picture
renderDecoration maybeProp = case maybeProp of
  Nothing -> blank
  Just PropPlant -> Color green $ circleSolid (tileSize * 0.18)
  Just PropSign -> Color yellow $ rectangleSolid (tileSize * 0.16) (tileSize * 0.52)
  Just PropRubble -> Color (greyN 0.65) $ circleSolid (tileSize * 0.12)
