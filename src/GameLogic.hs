module GameLogic
  ( updateWorld
  , movePlayer
  , playerSpeed
  ) where

import Types

-- | Player movement speed in pixels per second.
playerSpeed :: Float
playerSpeed = 150.0

-- | Advance the game world by @dt@ seconds.
--
--   Currently applies movement and increments the world clock.
updateWorld :: Float -> GameWorld -> GameWorld
updateWorld dt world =
  let world' = movePlayer dt world
  in  world' { worldTime = worldTime world' + dt }

-- | Move the player according to the current 'InputState'.
--
--   This is a pure function: no IO, no side effects.
movePlayer :: Float -> GameWorld -> GameWorld
movePlayer dt world =
  let input  = worldInput world
      player = worldPlayer world
      (px, py) = playerPos player

      dx = (if keyRight input then playerSpeed else 0)
         - (if keyLeft  input then playerSpeed else 0)
      dy = (if keyUp    input then playerSpeed else 0)
         - (if keyDown  input then playerSpeed else 0)

      newPos = (px + dx * dt, py + dy * dt)

      newDir
        | keyUp    input = DirUp
        | keyDown  input = DirDown
        | keyLeft  input = DirLeft
        | keyRight input = DirRight
        | otherwise      = playerDirection player

      player' = player
        { playerPos       = newPos
        , playerDirection = newDir
        }
  in world { worldPlayer = player' }
