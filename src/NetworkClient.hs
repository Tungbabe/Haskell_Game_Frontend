module NetworkClient
  ( NetworkConfig(..)
  , defaultNetworkConfig
  , connectToServer
  , sendPlayerState
  , receiveGameState
  ) where

import Types
import Network.Socket (Socket)

-- | Server connection parameters.
data NetworkConfig = NetworkConfig
  { serverHost :: String
  , serverPort :: Int
  } deriving (Show, Eq)

-- | Default configuration pointing to localhost:9000.
defaultNetworkConfig :: NetworkConfig
defaultNetworkConfig = NetworkConfig
  { serverHost = "127.0.0.1"
  , serverPort = 9000
  }

-- | Attempt to connect to the game server.
--
--   /Placeholder/ — always returns 'Nothing' for now.
connectToServer :: NetworkConfig -> IO (Maybe Socket)
connectToServer _config = do
  putStrLn "[Network] Connection placeholder — not yet implemented."
  return Nothing

-- | Send the local player state over the network.
--
--   /Placeholder/ — silently no‑ops when there is no socket.
sendPlayerState :: Maybe Socket -> Player -> IO ()
sendPlayerState Nothing  _      = return ()
sendPlayerState (Just _) _player = do
  putStrLn "[Network] Sending player state (placeholder)."
  return ()

-- | Receive other players' state from the server.
--
--   /Placeholder/ — always returns 'Nothing'.
receiveGameState :: Maybe Socket -> IO (Maybe [Player])
receiveGameState Nothing  = return Nothing
receiveGameState (Just _) = do
  putStrLn "[Network] Receiving game state (placeholder)."
  return Nothing
