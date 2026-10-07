module Main where

import Graphics.Gloss                     (Display(..), Color, makeColorI)
import Graphics.Gloss.Interface.Pure.Game (play)

import qualified Data.Map.Strict as Map

import Types
import AssetLoader   (loadAssets)
import Input         (handleInput)
import GameLogic     (updateWorld)
import Render.Core   (renderWorld, windowWidth, windowHeight)

-- ---------------------------------------------------------------------------
-- Window & display settings
-- ---------------------------------------------------------------------------

-- | The Gloss window descriptor.
window :: Display
window = InWindow "Haskell Game Frontend" (windowWidth, windowHeight) (100, 100)

-- | Dark background colour.
bgColor :: Color
bgColor = makeColorI 30 30 46 255

-- | Target frames per second.
fps :: Int
fps = 60

-- ---------------------------------------------------------------------------
-- Sample map
-- ---------------------------------------------------------------------------

-- | A small 11×11 room with walls around the edges.
sampleMap :: GameMap
sampleMap = Map.fromList
  [ ((x, y), tile)
  | x <- [-5 .. 5]
  , y <- [-5 .. 5]
  , let tile
          | x == -5 || x == 5 || y == -5 || y == 5 = Wall
          | otherwise                                = Floor
  ]

-- ---------------------------------------------------------------------------
-- Entry point
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Loading assets..."
  assets <- loadAssets
  putStrLn "Assets loaded successfully!"

  let initialWorld = mkGameWorld assets sampleMap

  putStrLn $ "Starting game at " ++ show fps ++ " FPS..."
  play window bgColor fps initialWorld renderWorld handleInput updateWorld
