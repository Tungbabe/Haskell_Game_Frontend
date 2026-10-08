{-# LANGUAGE ScopedTypeVariables #-}

module Server (runServer) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, finally, try)
import Control.Monad (forM_, forever, void, when)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GameLogic
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(AI_PASSIVE)
  , Socket
  , SocketOption(ReuseAddr)
  , SocketType(Stream)
  , accept
  , bind
  , close
  , defaultHints
  , defaultProtocol
  , getAddrInfo
  , listen
  , setSocketOption
  , socket
  , socketToHandle
  , withSocketsDo
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

data ClientSession = ClientSession
  { sessionResponses :: TChan ServerResponse
  }

data ServerState = ServerState
  { serverRooms :: Map.Map RoomCode GameRoom
  , serverMatches :: Map.Map RoomCode GameWorld
  , serverPlayerRooms :: Map.Map PlayerId RoomCode
  , serverSessions :: Map.Map PlayerId ClientSession
  , serverNextPlayerNumber :: Int
  , serverNextRoomNumber :: Int
  }

emptyServerState :: ServerState
emptyServerState = ServerState
  { serverRooms = Map.empty
  , serverMatches = Map.empty
  , serverPlayerRooms = Map.empty
  , serverSessions = Map.empty
  , serverNextPlayerNumber = 1
  , serverNextRoomNumber = 1
  }

serverTickMicroseconds :: Int
serverTickMicroseconds = 33333

serverTickSeconds :: Float
serverTickSeconds = 1 / 30

runServer :: String -> IO ()
runServer port = withSocketsDo $ do
  stateVar <- newTVarIO emptyServerState
  void $ forkIO $ serverTickLoop stateVar
  address <- resolveAddress port
  listenSocket <- openListenSocket address
  putStrLn $ "Tank server listening on port " ++ port
  forever $ do
    (clientSocket, _) <- accept listenSocket
    void $ forkIO $ serveClient stateVar clientSocket

resolveAddress :: String -> IO AddrInfo
resolveAddress port = do
  addresses <- getAddrInfo
    (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream })
    Nothing
    (Just port)
  case addresses of
    [] -> ioError $ userError "No listen address resolved"
    address : _ -> pure address

openListenSocket :: AddrInfo -> IO Socket
openListenSocket address = do
  listenSocket <- socket (addrFamily address) Stream defaultProtocol
  setSocketOption listenSocket ReuseAddr 1
  bind listenSocket (addrAddress address)
  listen listenSocket 16
  pure listenSocket

serveClient :: TVar ServerState -> Socket -> IO ()
serveClient stateVar clientSocket = do
  handle <- socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering
  (playerId, responses) <- atomically $ registerClient stateVar
  writerThread <- forkIO $ responseWriter handle responses
  atomically $ sendToPlayer stateVar playerId (ResConnected playerId)
  let readLoop = forever $ do
        received <- try (hGetLine handle) :: IO (Either IOException String)
        case received of
          Left _ -> ioError $ userError "Client disconnected"
          Right line -> case decodeClientCommand line of
            Nothing -> atomically $ reject stateVar playerId "Invalid command"
            Just command -> atomically $ handleCommand stateVar playerId command
  readLoop `finally` do
    killThread writerThread
    hClose handle
    atomically $ disconnectClient stateVar playerId

responseWriter :: Handle -> TChan ServerResponse -> IO ()
responseWriter handle responses = forever $ do
  response <- atomically $ readTChan responses
  hPutStrLn handle $ show response
  hFlush handle

decodeClientCommand :: String -> Maybe ClientCommand
decodeClientCommand line = case
  [ command
  | (command, remainder) <- reads line
  , all isSpace remainder
  ] of
    [command] -> Just command
    _ -> Nothing

registerClient :: TVar ServerState -> STM (PlayerId, TChan ServerResponse)
registerClient stateVar = do
  state <- readTVar stateVar
  responses <- newTChan
  let playerId = PlayerId $ "player-" ++ show (serverNextPlayerNumber state)
      session = ClientSession responses
      nextState = state
        { serverSessions = Map.insert playerId session (serverSessions state)
        , serverNextPlayerNumber = serverNextPlayerNumber state + 1
        }
  writeTVar stateVar nextState
  pure (playerId, responses)

disconnectClient :: TVar ServerState -> PlayerId -> STM ()
disconnectClient stateVar playerId = do
  state <- readTVar stateVar
  let withoutSession = state
        { serverSessions = Map.delete playerId (serverSessions state) }
  writeTVar stateVar withoutSession
  removePlayerFromRoom stateVar playerId

handleCommand :: TVar ServerState -> PlayerId -> ClientCommand -> STM ()
handleCommand stateVar playerId command = case command of
  CmdCreateRoom playerName requestedCapacity -> createRoom stateVar playerId playerName requestedCapacity
  CmdJoinRoom roomCode playerName -> joinRoom stateVar playerId roomCode playerName
  CmdLeaveRoom -> removePlayerFromRoom stateVar playerId
  CmdSelectTank tankType tankColor -> selectTank stateVar playerId tankType tankColor
  CmdSetReady isReady -> setReady stateVar playerId isReady
  CmdSelectMap mapType -> selectMap stateVar playerId mapType
  CmdStartMatch -> startMatch stateVar playerId
  CmdInput inputState -> applyMatchInput stateVar playerId inputState
  CmdPlayAgain -> resetRoomForReplay stateVar playerId

createRoom :: TVar ServerState -> PlayerId -> String -> Int -> STM ()
createRoom stateVar playerId playerName requestedCapacity = do
  state <- readTVar stateVar
  if Map.member playerId (serverPlayerRooms state)
    then reject stateVar playerId "Leave the current room before creating another one"
    else do
      let code = RoomCode $ roomCodeText (serverNextRoomNumber state)
          capacity = max 2 $ min 3 requestedCapacity
          roomPlayer = RoomPlayer playerId (normaliseName playerName) Nothing Nothing False
          room = GameRoom
            { roomCode = code
            , roomHostId = playerId
            , roomPlayers = Map.singleton playerId roomPlayer
            , roomSelectedMap = Just MapDepot
            , roomMaxPlayers = capacity
            , roomStatus = RoomLobby
            }
          nextState = state
            { serverRooms = Map.insert code room (serverRooms state)
            , serverPlayerRooms = Map.insert playerId code (serverPlayerRooms state)
            , serverNextRoomNumber = serverNextRoomNumber state + 1
            }
      writeTVar stateVar nextState
      sendToPlayer stateVar playerId $ ResRoomCreated room
      broadcastRoom stateVar room $ ResRoomUpdated room

joinRoom :: TVar ServerState -> PlayerId -> RoomCode -> String -> STM ()
joinRoom stateVar playerId code playerName = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverPlayerRooms state) of
    Just currentCode
      | currentCode /= code -> reject stateVar playerId "Leave the current room before joining another one"
    _ -> case Map.lookup code (serverRooms state) of
      Nothing -> reject stateVar playerId "Room code not found"
      Just room
        | roomStatus room /= RoomLobby -> reject stateVar playerId "This match has already started"
        | Map.member playerId (roomPlayers room) -> do
            sendToPlayer stateVar playerId $ ResRoomUpdated room
        | Map.size (roomPlayers room) >= roomMaxPlayers room ->
            reject stateVar playerId "Room is full"
        | otherwise -> do
            let roomPlayer = RoomPlayer playerId (normaliseName playerName) Nothing Nothing False
                updatedRoom = room
                  { roomPlayers = Map.insert playerId roomPlayer (roomPlayers room) }
                nextState = state
                  { serverRooms = Map.insert code updatedRoom (serverRooms state)
                  , serverPlayerRooms = Map.insert playerId code (serverPlayerRooms state)
                  }
            writeTVar stateVar nextState
            broadcastRoom stateVar updatedRoom $ ResRoomUpdated updatedRoom

selectTank :: TVar ServerState -> PlayerId -> TankType -> TankColor -> STM ()
selectTank stateVar playerId tankType tankColor = withLobbyRoom stateVar playerId $ \room player -> do
  let duplicateColor = any usesColor $ Map.elems (roomPlayers room)
      usesColor other = roomPlayerId other /= playerId && roomPlayerTankColor other == Just tankColor
  if duplicateColor
    then reject stateVar playerId "That tank color is already selected"
    else do
      let updatedPlayer = player
            { roomPlayerTankType = Just tankType
            , roomPlayerTankColor = Just tankColor
            , roomPlayerReady = False
            }
          updatedRoom = room
            { roomPlayers = Map.insert playerId updatedPlayer (roomPlayers room) }
      replaceRoom stateVar updatedRoom
      broadcastRoom stateVar updatedRoom $ ResRoomUpdated updatedRoom

setReady :: TVar ServerState -> PlayerId -> Bool -> STM ()
setReady stateVar playerId isReady = withLobbyRoom stateVar playerId $ \room player ->
  case (roomPlayerTankType player, roomPlayerTankColor player) of
    (Just _, Just _) -> do
      let updatedPlayer = player { roomPlayerReady = isReady }
          updatedRoom = room
            { roomPlayers = Map.insert playerId updatedPlayer (roomPlayers room) }
      replaceRoom stateVar updatedRoom
      broadcastRoom stateVar updatedRoom $ ResRoomUpdated updatedRoom
    _ -> reject stateVar playerId "Choose a tank type and color before readying up"

selectMap :: TVar ServerState -> PlayerId -> MapType -> STM ()
selectMap stateVar playerId mapType = withLobbyRoom stateVar playerId $ \room _ ->
  if roomHostId room /= playerId
    then reject stateVar playerId "Only the host can choose the map"
    else do
      let updatedRoom = room { roomSelectedMap = Just mapType }
      replaceRoom stateVar updatedRoom
      broadcastRoom stateVar updatedRoom $ ResRoomUpdated updatedRoom

startMatch :: TVar ServerState -> PlayerId -> STM ()
startMatch stateVar playerId = withLobbyRoom stateVar playerId $ \room _ ->
  if roomHostId room /= playerId
    then reject stateVar playerId "Only the host can start the match"
    else if not (roomCanStart room)
      then reject stateVar playerId "Need 2-3 ready players with unique tank colors"
      else do
        let mapType = fromMaybe MapDepot (roomSelectedMap room)
            gameMap = gameMapFor mapType
            gameWorld = makeGameWorld room mapType gameMap
            activeRoom = room { roomStatus = RoomPlaying }
        state <- readTVar stateVar
        writeTVar stateVar state
          { serverRooms = Map.insert (roomCode room) activeRoom (serverRooms state)
          , serverMatches = Map.insert (roomCode room) gameWorld (serverMatches state)
          }
        broadcastRoom stateVar activeRoom $ ResMatchStarted gameWorld

applyMatchInput :: TVar ServerState -> PlayerId -> InputState -> STM ()
applyMatchInput stateVar playerId inputState = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverPlayerRooms state) >>= (`Map.lookup` serverMatches state) of
    Nothing -> reject stateVar playerId "You are not in an active match"
    Just previousWorld -> do
      let updatedWorld = applyClientCommand playerId (CmdInput inputState) previousWorld
          pickedItems = removedItems (worldItems previousWorld) (worldItems updatedWorld)
          shotWasFired = length (worldBullets updatedWorld) > length (worldBullets previousWorld)
          code = worldRoomCode updatedWorld
      writeTVar stateVar state
        { serverMatches = Map.insert code updatedWorld (serverMatches state) }
      when shotWasFired $ broadcastMatch stateVar updatedWorld $ ResAudio SfxShoot
      forM_ pickedItems $ \item ->
        broadcastMatch stateVar updatedWorld $ ResAudio (SfxItemPickup $ itemType item)
      broadcastMatch stateVar updatedWorld $ ResWorldSnapshot updatedWorld

resetRoomForReplay :: TVar ServerState -> PlayerId -> STM ()
resetRoomForReplay stateVar playerId = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverPlayerRooms state) >>= (`Map.lookup` serverRooms state) of
    Nothing -> reject stateVar playerId "You are not in a room"
    Just room
      | roomStatus room /= RoomFinished -> reject stateVar playerId "The current match has not finished"
      | otherwise -> do
          let resetPlayers = Map.map (\player -> player { roomPlayerReady = False }) (roomPlayers room)
              resetRoom = room { roomStatus = RoomLobby, roomPlayers = resetPlayers }
          writeTVar stateVar state
            { serverRooms = Map.insert (roomCode room) resetRoom (serverRooms state)
            , serverMatches = Map.delete (roomCode room) (serverMatches state)
            }
          broadcastRoom stateVar resetRoom $ ResRoomUpdated resetRoom

withLobbyRoom
  :: TVar ServerState
  -> PlayerId
  -> (GameRoom -> RoomPlayer -> STM ())
  -> STM ()
withLobbyRoom stateVar playerId action = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverPlayerRooms state) >>= (`Map.lookup` serverRooms state) of
    Nothing -> reject stateVar playerId "You are not in a room"
    Just room
      | roomStatus room /= RoomLobby -> reject stateVar playerId "This action is only available in the lobby"
      | otherwise -> case Map.lookup playerId (roomPlayers room) of
          Nothing -> reject stateVar playerId "Room membership is invalid"
          Just player -> action room player

replaceRoom :: TVar ServerState -> GameRoom -> STM ()
replaceRoom stateVar room = do
  state <- readTVar stateVar
  writeTVar stateVar state
    { serverRooms = Map.insert (roomCode room) room (serverRooms state) }

removePlayerFromRoom :: TVar ServerState -> PlayerId -> STM ()
removePlayerFromRoom stateVar playerId = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverPlayerRooms state) >>= (`Map.lookup` serverRooms state) of
    Nothing -> pure ()
    Just room -> do
      let code = roomCode room
          remainingPlayers = Map.delete playerId (roomPlayers room)
          playerRoomsWithoutPlayer = Map.delete playerId (serverPlayerRooms state)
          remainingWorld = Map.adjust removeFromWorld code (serverMatches state)
          removeFromWorld world = world
            { worldPlayers = Map.delete playerId (worldPlayers world) }
      if Map.null remainingPlayers
        then writeTVar stateVar state
          { serverRooms = Map.delete code (serverRooms state)
          , serverMatches = Map.delete code (serverMatches state)
          , serverPlayerRooms = playerRoomsWithoutPlayer
          }
        else do
          let replacementHost = if roomHostId room == playerId
                then fst (Map.findMin remainingPlayers)
                else roomHostId room
              updatedRoom = room
                { roomHostId = replacementHost
                , roomPlayers = remainingPlayers
                }
              nextState = state
                { serverRooms = Map.insert code updatedRoom (serverRooms state)
                , serverMatches = remainingWorld
                , serverPlayerRooms = playerRoomsWithoutPlayer
                }
          writeTVar stateVar nextState
          broadcastRoom stateVar updatedRoom $ ResRoomUpdated updatedRoom
          case Map.lookup code remainingWorld of
            Just world | roomStatus updatedRoom == RoomPlaying ->
              broadcastMatch stateVar world $ ResWorldSnapshot world
            _ -> pure ()

serverTickLoop :: TVar ServerState -> IO ()
serverTickLoop stateVar = forever $ do
  threadDelay serverTickMicroseconds
  atomically $ advanceMatches stateVar

advanceMatches :: TVar ServerState -> STM ()
advanceMatches stateVar = do
  state <- readTVar stateVar
  let activeMatches = Map.filter ((== MatchRunning) . worldStatus) (serverMatches state)
      advancedMatches = Map.map (stepWorld serverTickSeconds) activeMatches
      retainedMatches = Map.union advancedMatches (Map.filter ((/= MatchRunning) . worldStatus) (serverMatches state))
  writeTVar stateVar state { serverMatches = retainedMatches }
  forM_ (Map.toList advancedMatches) $ \(roomCode, world) -> do
    let previousWorld = Map.lookup roomCode (serverMatches state)
        deathsThisTick = maybe [] (newlyDead world) previousWorld
        finished = not (Map.null $ worldPlayers world) && length (livingPlayers world) <= 1
    if finished
      then finishMatch stateVar world
      else do
        broadcastMatch stateVar world $ ResWorldSnapshot world
        when (length (worldBombs world) < maybe (length $ worldBombs world) (length . worldBombs) previousWorld) $
          broadcastMatch stateVar world $ ResAudio SfxBombExplosion
        when (not $ null deathsThisTick) $ do
          broadcastMatch stateVar world $ ResAudio SfxDeath
          broadcastMatch stateVar world $ ResAudio SfxKill
  where
    newlyDead currentWorld previousWorld =
      [ player
      | player <- Map.elems (worldPlayers currentWorld)
      , playerStatus player == PlayerDead
      , maybe False ((== PlayerAlive) . playerStatus) $ Map.lookup (playerId player) (worldPlayers previousWorld)
      ]

finishMatch :: TVar ServerState -> GameWorld -> STM ()
finishMatch stateVar world = do
  state <- readTVar stateVar
  case Map.lookup (worldRoomCode world) (serverRooms state) of
    Nothing -> pure ()
    Just room -> do
      let finalWorld = world { worldStatus = MatchComplete }
          finishedRoom = room { roomStatus = RoomFinished }
          result = matchResultFor finalWorld
      writeTVar stateVar state
        { serverRooms = Map.insert (roomCode room) finishedRoom (serverRooms state)
        , serverMatches = Map.insert (roomCode room) finalWorld (serverMatches state)
        }
      broadcastMatch stateVar finalWorld $ ResWorldSnapshot finalWorld
      broadcastRoom stateVar finishedRoom $ ResMatchEnded result

sendToPlayer :: TVar ServerState -> PlayerId -> ServerResponse -> STM ()
sendToPlayer stateVar playerId response = do
  state <- readTVar stateVar
  case Map.lookup playerId (serverSessions state) of
    Nothing -> pure ()
    Just session -> writeTChan (sessionResponses session) response

broadcastRoom :: TVar ServerState -> GameRoom -> ServerResponse -> STM ()
broadcastRoom stateVar room response =
  forM_ (Map.keys $ roomPlayers room) $ \playerId ->
    sendToPlayer stateVar playerId response

broadcastMatch :: TVar ServerState -> GameWorld -> ServerResponse -> STM ()
broadcastMatch stateVar world response =
  forM_ (Map.keys $ worldPlayers world) $ \playerId ->
    sendToPlayer stateVar playerId response

reject :: TVar ServerState -> PlayerId -> String -> STM ()
reject stateVar playerId message =
  sendToPlayer stateVar playerId $ ResCommandRejected message

roomCanStart :: GameRoom -> Bool
roomCanStart room =
  playerCount >= 2
    && playerCount <= roomMaxPlayers room
    && all fullySelected roomPlayerList
    && all roomPlayerReady roomPlayerList
    && colorsAreUnique roomPlayerList
  where
    roomPlayerList = Map.elems (roomPlayers room)
    playerCount = length roomPlayerList
    fullySelected player = case (roomPlayerTankType player, roomPlayerTankColor player) of
      (Just _, Just _) -> True
      _ -> False
    colorsAreUnique players =
      let selectedColors = [ color | player <- players, Just color <- [roomPlayerTankColor player] ]
      in length selectedColors == length (unique selectedColors)

unique :: Eq a => [a] -> [a]
unique = foldr addIfMissing []
  where
    addIfMissing value values
      | value `elem` values = values
      | otherwise = value : values

makeGameWorld :: GameRoom -> MapType -> GameMap -> GameWorld
makeGameWorld room mapType gameMap = GameWorld
  { worldRoomCode = roomCode room
  , worldMapType = mapType
  , worldMap = gameMap
  , worldPlayers = Map.fromList $ zipWith makePlayer (Map.elems $ roomPlayers room) spawnPositions
  , worldBullets = []
  , worldItems = initialItemsFor mapType gameMap
  , worldBombs = []
  , worldElapsedSeconds = 0
  , worldStatus = MatchRunning
  }
  where
    makePlayer roomPlayer position =
      ( roomPlayerId roomPlayer
      , Player
          { playerId = roomPlayerId roomPlayer
          , playerName = roomPlayerName roomPlayer
          , playerTankType = fromMaybe TankScout (roomPlayerTankType roomPlayer)
          , playerTankColor = fromMaybe TankRed (roomPlayerTankColor roomPlayer)
          , playerPosition = position
          , playerBodyDirection = DirUp
          , playerBodyAngle = 0
          , playerTurretAngle = 0
          , playerHealth = 100
          , playerMaxHealth = 100
          , playerStatus = PlayerAlive
          , playerShieldRemaining = 0
          , playerFireCooldown = 0
          , playerBombCount = 0
          , playerKills = 0
          , playerScore = 0
          }
      )

removedItems :: [Item] -> [Item] -> [Item]
removedItems before after = filter (`notElem` after) before

normaliseName :: String -> String
normaliseName name
  | null trimmed = "Player"
  | otherwise = take 16 trimmed
  where
    trimmed = filter (not . isSpace) name

roomCodeText :: Int -> String
roomCodeText number = "R" ++ padToFive (show number)

padToFive :: String -> String
padToFive digits = replicate (max 0 $ 5 - length digits) '0' ++ digits
