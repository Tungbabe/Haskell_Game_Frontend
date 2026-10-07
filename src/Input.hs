module Input
  ( handleInput
  ) where

import Types
import Graphics.Gloss.Interface.Pure.Game
    ( Event(..)
    , Key(..)
    , SpecialKey(..)
    , KeyState(..)
    )

-- | Map a Gloss 'Event' to an updated 'GameWorld'.
--
--   Arrow keys control movement; Space triggers the shoot action.
--   All other events are ignored.
handleInput :: Event -> GameWorld -> GameWorld
handleInput (EventKey key keyState _ _) world =
  let pressed = keyState == Down
      input   = worldInput world
      input'  = case key of
        SpecialKey KeyUp    -> input { keyUp    = pressed }
        SpecialKey KeyDown  -> input { keyDown  = pressed }
        SpecialKey KeyLeft  -> input { keyLeft  = pressed }
        SpecialKey KeyRight -> input { keyRight = pressed }
        Char ' '            -> input { keyShoot = pressed }
        _                   -> input
  in world { worldInput = input' }
handleInput _ world = world
