module Types where

import           Graphics.Gloss
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Direction & Position
-- ---------------------------------------------------------------------------

-- | Cardinal direction the player is facing.
data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Read, Eq, Ord)

-- | 2‑D position in world coordinates.
type Pos = (Float, Float)

-- ---------------------------------------------------------------------------
-- Map tiles
-- ---------------------------------------------------------------------------

-- | The different kinds of tile that can appear on the map.
data Tile = Wall | Floor | Empty
  deriving (Show, Read, Eq, Ord)

-- | A game map: sparse grid of tiles keyed by (column, row).
type GameMap = Map.Map (Int, Int) Tile

-- ---------------------------------------------------------------------------
-- Player
-- ---------------------------------------------------------------------------

-- | State of a single player.
data Player = Player
  { playerPos       :: !Pos
  , playerDirection :: !Direction
  , playerHealth    :: !Int
  , playerName      :: !String
  } deriving (Show, Read, Eq)

-- ---------------------------------------------------------------------------
-- Assets
-- ---------------------------------------------------------------------------

-- | Collection of loaded image assets.
data Assets = Assets
  { assetPlayer :: Picture
  , assetWall   :: Picture
  }

-- Manual Show instance because Picture's default Show is verbose.
instance Show Assets where
  show _ = "Assets{..}"

-- ---------------------------------------------------------------------------
-- Input
-- ---------------------------------------------------------------------------

-- | Snapshot of which keys are currently held down.
data InputState = InputState
  { keyUp    :: !Bool
  , keyDown  :: !Bool
  , keyLeft  :: !Bool
  , keyRight :: !Bool
  , keyShoot :: !Bool
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Game World
-- ---------------------------------------------------------------------------

-- | The complete, top‑level game state.
data GameWorld = GameWorld
  { worldPlayer   :: !Player
  , worldOthers   :: ![Player]
  , worldMap      :: !GameMap
  , worldAssets   :: !Assets
  , worldInput    :: !InputState
  , worldTime     :: !Float
  , worldMessages :: ![String]
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Game Screen (application state machine)
-- ---------------------------------------------------------------------------

-- | Top‑level screen the application can be in.
data GameScreen
  = MainMenu              -- ^ Title / main‑menu screen
  | InGame   GameWorld     -- ^ Active gameplay
  | GameOver String        -- ^ End screen with a reason message
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Network Protocol
-- ---------------------------------------------------------------------------

-- | Commands the client sends to the server.
data ClientCommand
  = CmdMove      Direction         -- ^ Request to move in a direction
  | CmdShoot     Pos Direction     -- ^ Fire from position towards direction
  | CmdJoin      String            -- ^ Join lobby with player name
  | CmdLeave                       -- ^ Gracefully disconnect
  | CmdChat      String            -- ^ Send a chat message
  | CmdPing                        -- ^ Keep‑alive ping
  deriving (Show, Read, Eq)

-- | Responses / events the server sends back to clients.
data ServerResponse
  = RspWorldState  [Player] GameMap  -- ^ Full authoritative state snapshot
  | RspPlayerJoined String Pos       -- ^ Another player joined at position
  | RspPlayerLeft   String           -- ^ A player disconnected
  | RspDamage       String Int       -- ^ Player took damage (name, new HP)
  | RspChat         String String    -- ^ Chat message (sender, text)
  | RspGameOver     String           -- ^ Game ended with reason
  | RspPong                          -- ^ Reply to CmdPing
  deriving (Show, Read, Eq)

-- ---------------------------------------------------------------------------
-- Defaults & Constructors
-- ---------------------------------------------------------------------------

-- | No keys pressed.
defaultInputState :: InputState
defaultInputState = InputState False False False False False

-- | A fresh player at the origin.
defaultPlayer :: Player
defaultPlayer = Player
  { playerPos       = (0, 0)
  , playerDirection = DirDown
  , playerHealth    = 100
  , playerName      = "Player1"
  }

-- | Build an initial 'GameWorld' from loaded assets and a map.
mkGameWorld :: Assets -> GameMap -> GameWorld
mkGameWorld assets gameMap = GameWorld
  { worldPlayer   = defaultPlayer
  , worldOthers   = []
  , worldMap      = gameMap
  , worldAssets   = assets
  , worldInput    = defaultInputState
  , worldTime     = 0
  , worldMessages = ["Welcome to the game!"]
  }
