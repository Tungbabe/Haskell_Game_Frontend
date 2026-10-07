module Main where

import Test.Hspec
import Types
import GameLogic

-- | A dummy 'Assets' value that must not be evaluated during pure tests.
--   Movement logic never touches assets, so this is safe.
dummyAssets :: Assets
dummyAssets = error "Assets should not be evaluated in pure logic tests"

-- | Build a minimal 'GameWorld' for testing purposes.
mkTestWorld :: InputState -> GameWorld
mkTestWorld input = GameWorld
  { worldPlayer   = defaultPlayer
  , worldOthers   = []
  , worldMap      = mempty
  , worldAssets   = dummyAssets
  , worldInput    = input
  , worldTime     = 0
  , worldMessages = []
  }

main :: IO ()
main = hspec $ do

  -- -----------------------------------------------------------------------
  describe "Types — defaults" $ do

    it "defaultPlayer starts at the origin" $
      playerPos defaultPlayer `shouldBe` (0, 0)

    it "defaultPlayer has 100 HP" $
      playerHealth defaultPlayer `shouldBe` 100

    it "defaultInputState has all keys released" $ do
      let s = defaultInputState
      keyUp    s `shouldBe` False
      keyDown  s `shouldBe` False
      keyLeft  s `shouldBe` False
      keyRight s `shouldBe` False
      keyShoot s `shouldBe` False

  -- -----------------------------------------------------------------------
  describe "GameLogic — movement" $ do

    it "playerSpeed is positive" $
      playerSpeed `shouldSatisfy` (> 0)

    it "keeps position when no keys are pressed" $ do
      let world  = mkTestWorld defaultInputState
          world' = movePlayer 1.0 world
      playerPos (worldPlayer world') `shouldBe` (0, 0)

    it "moves right when keyRight is pressed" $ do
      let input  = defaultInputState { keyRight = True }
          world  = mkTestWorld input
          world' = movePlayer 1.0 world
          (px, _) = playerPos (worldPlayer world')
      px `shouldSatisfy` (> 0)

    it "moves up when keyUp is pressed" $ do
      let input  = defaultInputState { keyUp = True }
          world  = mkTestWorld input
          world' = movePlayer 1.0 world
          (_, py) = playerPos (worldPlayer world')
      py `shouldSatisfy` (> 0)

    it "increments worldTime via updateWorld" $ do
      let world  = mkTestWorld defaultInputState
          world' = updateWorld 0.5 world
      worldTime world' `shouldBe` 0.5
