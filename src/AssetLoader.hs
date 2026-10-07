module AssetLoader
  ( loadAssets
  ) where

import Types
import Graphics.Gloss (loadBMP)

-- | Load all game assets from the @assets/@ directory.
--
--   The BMP files must exist before calling this function.
--   Gloss expects uncompressed 24‑ or 32‑bit BMP files.
loadAssets :: IO Assets
loadAssets = do
  playerPic <- loadBMP "assets/player.bmp"
  wallPic   <- loadBMP "assets/wall.bmp"
  return Assets
    { assetPlayer = playerPic
    , assetWall   = wallPic
    }
