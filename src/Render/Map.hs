module Render.Map
  ( renderMap
  , renderTile
  , tileSize
  ) where

import Types
import Graphics.Gloss
import qualified Data.Map.Strict as Map

-- | Side length of each square tile in pixels.
tileSize :: Float
tileSize = 32.0

-- | Render every tile in the 'GameMap'.  Pure function.
renderMap :: GameMap -> Assets -> Picture
renderMap gameMap assets =
  Pictures $ map (renderTileAt assets) (Map.toList gameMap)

-- | Place a single tile at its grid position.  Pure function.
renderTileAt :: Assets -> ((Int, Int), Tile) -> Picture
renderTileAt assets ((gx, gy), tile) =
  let x = fromIntegral gx * tileSize
      y = fromIntegral gy * tileSize
  in  Translate x y (renderTile tile assets)

-- | Convert a 'Tile' to its visual representation.  Pure function.
renderTile :: Tile -> Assets -> Picture
renderTile Wall  assets = assetWall assets
renderTile Floor _      = Color (greyN 0.8) $ rectangleSolid tileSize tileSize
renderTile Empty _      = Blank
