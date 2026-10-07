module Render.Core
  ( renderWorld
  , windowWidth
  , windowHeight
  ) where

import Types
import Graphics.Gloss
import Render.Map (renderMap)
import Render.UI  (renderUI)

-- | Window dimensions in pixels.
windowWidth, windowHeight :: Int
windowWidth  = 800
windowHeight = 600

-- | Render the entire game world.  Pure function.
renderWorld :: GameWorld -> Picture
renderWorld world = Pictures
  [ renderMap          (worldMap world)   (worldAssets world)
  , renderPlayer       (worldPlayer world) (worldAssets world)
  , renderOtherPlayers (worldOthers world) (worldAssets world)
  , renderUI world
  ]

-- | Draw the local player sprite at its position.  Pure function.
renderPlayer :: Player -> Assets -> Picture
renderPlayer player assets =
  let (px, py) = playerPos player
  in  Translate px py (assetPlayer assets)

-- | Draw every remote player.  Pure function.
renderOtherPlayers :: [Player] -> Assets -> Picture
renderOtherPlayers players assets =
  Pictures $ map (`renderPlayer` assets) players
