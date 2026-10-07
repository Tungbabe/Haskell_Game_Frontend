module Render.UI
  ( renderUI
  , renderHealthBar
  , renderMessages
  ) where

import Types
import Graphics.Gloss

-- | Compose the full HUD overlay.  Pure function.
renderUI :: GameWorld -> Picture
renderUI world = Pictures
  [ renderHealthBar  (worldPlayer world)
  , renderMessages   (worldMessages world)
  , renderPlayerInfo (worldPlayer world)
  ]

-- | Draw a horizontal health bar in the top‑left corner.  Pure function.
renderHealthBar :: Player -> Picture
renderHealthBar player =
  let hp        = fromIntegral (playerHealth player) :: Float
      maxHp     = 100.0
      barWidth  = 200.0
      barHeight = 20.0
      fillWidth = barWidth * (hp / maxHp)
      xOff      = -370.0
      yOff      =  270.0
  in Translate xOff yOff $ Pictures
       [ -- background
         Color (greyN 0.3) $ rectangleSolid barWidth barHeight
         -- health fill
       , Translate (-(barWidth - fillWidth) / 2) 0 $
           Color green $ rectangleSolid fillWidth barHeight
         -- label
       , Translate (-barWidth / 2 - 30) (-5) $
           Scale 0.1 0.1 $ Color white $ Text "HP"
       ]

-- | Show the most recent system / chat messages at the bottom.  Pure function.
renderMessages :: [String] -> Picture
renderMessages msgs =
  let yStart   = -250.0
      rendered = zipWith
        (\i msg ->
          Translate (-380) (yStart - fromIntegral i * 18) $
            Scale 0.1 0.1 $ Color (greyN 0.9) $ Text msg
        )
        [0 :: Int ..]
        (take 5 msgs)
  in Pictures rendered

-- | Display the player's name and rounded coordinates.  Pure function.
renderPlayerInfo :: Player -> Picture
renderPlayerInfo player =
  let (px, py) = playerPos player
      info     = playerName player
              ++ " ("  ++ show (round px :: Int)
              ++ ", "  ++ show (round py :: Int)
              ++ ")"
  in Translate (-380) 250 $
       Scale 0.1 0.1 $ Color white $ Text info
