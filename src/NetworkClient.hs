{-# LANGUAGE ScopedTypeVariables #-}

module NetworkClient
  ( ClientConnection
  , startNetworkClient
  , sendCommand
  , closeConnection
  , decodeServerResponse
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (IOException, finally, onException, try)
import Control.Monad (forever)
import Data.Char (isSpace)
import Network.Socket
  ( AddrInfo(..)
  , SocketType(Stream)
  , close
  , connect
  , defaultProtocol
  , getAddrInfo
  , socket
  , socketToHandle
  )
import System.IO
  ( BufferMode(LineBuffering)
  , Handle
  , IOMode(ReadWriteMode)
  , hClose
  , hFlush
  , hGetLine
  , hPutStrLn
  , hSetBuffering
  )
import Types

-- | A connected client owns a thread-safe outbound command queue.
data ClientConnection = ClientConnection
  { outgoingCommands :: TChan ClientCommand
  , networkThread :: ThreadId
  }

-- | The network thread owns socket I/O. The Gloss callbacks only read or write
-- | TVars/TChans, so a slow connection cannot stall rendering.
startNetworkClient
  :: String
  -> String
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO (Maybe ClientConnection)
startNetworkClient host port screenVar playerIdVar messageVar audioChannel = do
  result <- try (openHandle host port) :: IO (Either IOException Handle)
  case result of
    Left _ -> do
      atomically $ writeTVar messageVar $ Just "Connection failed. Start game-server-exe and try again."
      pure Nothing
    Right handle -> do
      commands <- newTChanIO
      threadId <- forkIO $
        runClient handle commands screenVar playerIdVar messageVar audioChannel
          `finally` do
            hClose handle
            atomically $ writeTVar messageVar $ Just "Connection lost."
      pure $ Just ClientConnection
        { outgoingCommands = commands
        , networkThread = threadId
        }

sendCommand :: ClientConnection -> ClientCommand -> IO ()
sendCommand connection command =
  atomically $ writeTChan (outgoingCommands connection) command

closeConnection :: ClientConnection -> IO ()
closeConnection = killThread . networkThread

openHandle :: String -> String -> IO Handle
openHandle host port = do
  addresses <- getAddrInfo Nothing (Just host) (Just port)
  case addresses of
    [] -> ioError $ userError "No TCP address resolved for game server"
    serverAddress : _ -> do
      socketHandle <- socket (addrFamily serverAddress) Stream defaultProtocol
      connect socketHandle (addrAddress serverAddress) `onException` close socketHandle
      socketToHandle socketHandle ReadWriteMode `onException` close socketHandle

runClient
  :: Handle
  -> TChan ClientCommand
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO ()
runClient handle commands screenVar playerIdVar messageVar audioChannel = do
  hSetBuffering handle LineBuffering
  writerThread <- forkIO $ writerLoop handle commands
  receiveLoop handle screenVar playerIdVar messageVar audioChannel `finally` killThread writerThread

writerLoop :: Handle -> TChan ClientCommand -> IO ()
writerLoop handle commands = forever $ do
  command <- atomically $ readTChan commands
  hPutStrLn handle $ show command
  hFlush handle

receiveLoop
  :: Handle
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO ()
receiveLoop handle screenVar playerIdVar messageVar audioChannel = forever $ do
  responseLine <- hGetLine handle
  case decodeServerResponse responseLine of
    Just response -> handleResponse response screenVar playerIdVar messageVar audioChannel
    Nothing -> atomically $ writeTVar messageVar $ Just "Ignored an invalid server response."

-- | Decode exactly one newline-delimited protocol value.
decodeServerResponse :: String -> Maybe ServerResponse
decodeServerResponse line = case
  [ response
  | (response, remainder) <- reads line
  , all isSpace remainder
  ] of
    [response] -> Just response
    _ -> Nothing

handleResponse
  :: ServerResponse
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO ()
handleResponse response screenVar playerIdVar messageVar audioChannel = atomically $ case response of
  ResConnected playerId -> do
    writeTVar playerIdVar $ Just playerId
    writeTVar messageVar Nothing
  ResRoomCreated room -> do
    writeTVar screenVar $ ScreenLobby room
    writeTVar messageVar Nothing
  ResRoomUpdated room -> do
    writeTVar screenVar $ ScreenLobby room
    writeTVar messageVar Nothing
  ResMatchStarted world -> do
    writeTVar screenVar $ ScreenMatch world
    writeTChan audioChannel $ PlayMusic MusicMatch
    writeTVar messageVar Nothing
  ResWorldSnapshot world -> writeTVar screenVar $ ScreenMatch world
  ResMatchEnded result -> do
    localPlayerId <- readTVar playerIdVar
    writeTVar screenVar $ ScreenResults result
    writeTChan audioChannel $ PlaySound $ resultSound localPlayerId result
  ResAudio effect -> writeTChan audioChannel $ PlaySound effect
  ResCommandRejected reason -> writeTVar messageVar $ Just reason

resultSound :: Maybe PlayerId -> MatchResult -> SoundEffect
resultSound maybeLocalPlayer result = case (maybeLocalPlayer, resultWinnerId result) of
  (Just localPlayerId, Just winnerId)
    | localPlayerId == winnerId -> SfxVictory
  _ -> SfxDefeat
