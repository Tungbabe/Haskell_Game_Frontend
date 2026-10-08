module Main where

import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Char (isAlphaNum, toUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Graphics.Gloss.Interface.IO.Game
import GameLogic (aimAngleTo)
import NetworkClient
import Render.Camera
  ( Viewport(..)
  , cameraZoom
  , initialViewport
  , renderInVirtualCanvas
  , screenToWorld
  , windowToVirtual
  )
import Render.Core
import Render.UI
import Sound
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import Types

window :: Display
window = InWindow "Tank Arena" (windowWidth, windowHeight) (100, 100)

bgColor :: Color
bgColor = makeColorI 30 30 46 255

fallbackPlayerId :: PlayerId
fallbackPlayerId = PlayerId "local-preview"

-- | Every BMP is loaded once at startup and shared by every render call.
loadAssets :: IO Assets
loadAssets = do
  hull01 <- loadBMP "assets/images/Hull_01.bmp"
  gun01 <- loadBMP "assets/images/Gun_01.bmp"
  hull02 <- loadBMP "assets/images/Hull_02.bmp"
  gun06 <- loadBMP "assets/images/Gun_06.bmp"
  hull05 <- loadBMP "assets/images/Hull_05.bmp"
  gun05 <- loadBMP "assets/images/Gun_05.bmp"
  blueBullet <- loadBMP "assets/images/Bullet/Blue_Bullet.bmp"
  greenBullet <- loadBMP "assets/images/Bullet/Green_Bullet.bmp"
  redBullet <- loadBMP "assets/images/Bullet/Red_Bullet.bmp"
  explosionA <- loadBMP "assets/images/explosion/Explosion_A.bmp"
  explosionB <- loadBMP "assets/images/explosion/Explosion_B.bmp"
  explosionC <- loadBMP "assets/images/explosion/Explosion_C.bmp"
  explosionD <- loadBMP "assets/images/explosion/Explosion_D.bmp"
  explosionE <- loadBMP "assets/images/explosion/Explosion_E.bmp"
  explosionF <- loadBMP "assets/images/explosion/Explosion_F.bmp"
  explosionG <- loadBMP "assets/images/explosion/Explosion_G.bmp"
  explosionH <- loadBMP "assets/images/explosion/Explosion_H.bmp"
  menuBackground <- loadMenuBackground
  createRoomButton <- loadBMP "assets/images/batdauphong.bmp"
  joinRoomButton <- loadBMP "assets/images/timphong.bmp"
  exitButton <- loadBMP "assets/images/exit.bmp"
  depotButton <- loadBMP "assets/images/depot.bmp"
  forestButton <- loadBMP "assets/images/forest.bmp"
  ruinsButton <- loadBMP "assets/images/ruins.bmp"
  let bodyForTankType tankType = case tankType of
        TankScout -> hull01
        TankHeavy -> hull02
        TankArtillery -> hull05
      turretForTankType tankType = case tankType of
        TankScout -> gun01
        TankHeavy -> gun06
        TankArtillery -> gun05
      tankTypes = [TankScout, TankHeavy, TankArtillery]
      tankColors = [TankRed, TankBlue, TankGreen]
      directions = [DirUp, DirDown, DirLeft, DirRight]
      bodySprites = Map.fromList
        [ ((tankType, tankColor, direction), bodyForTankType tankType)
        | tankType <- tankTypes
        , tankColor <- tankColors
        , direction <- directions
        ]
      turretSprites = Map.fromList
        [ ((tankType, tankColor), turretForTankType tankType)
        | tankType <- tankTypes
        , tankColor <- tankColors
        ]
  pure Assets
    { assetsTankBodySprites = bodySprites
    , assetsTankTurretSprites = turretSprites
    , assetsTileSprites = Map.empty
    , assetsPropSprites = Map.empty
    , assetsItemSprites = Map.empty
    , assetsBulletSprites = Map.fromList
        [ (TankRed, redBullet)
        , (TankBlue, blueBullet)
        , (TankGreen, greenBullet)
        ]
    , assetsExplosionFrames =
        [ explosionA
        , explosionB
        , explosionC
        , explosionD
        , explosionE
        , explosionF
        , explosionG
        , explosionH
        ]
    , assetsMenuBackground = menuBackground
    , assetsCreateRoomButton = createRoomButton
    , assetsJoinRoomButton = joinRoomButton
    , assetsExitButton = exitButton
    , assetsMapButtonSprites = Map.fromList
        [ (MapDepot, depotButton)
        , (MapForest, forestButton)
        , (MapRuins, ruinsButton)
        ]
    , assetsUiSprites = Map.empty
    }

-- | The provided asset directory currently contains the historical spelling
-- | "Backgroup.bmp".  Prefer the requested corrected path when it is added.
loadMenuBackground :: IO Picture
loadMenuBackground = do
  hasCorrectedName <- doesFileExist "assets/images/Background.bmp"
  let backgroundPath
        | hasCorrectedName = "assets/images/Background.bmp"
        | otherwise = "assets/images/Backgroup.bmp"
  loadBMP backgroundPath

renderGame
  :: Assets
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar String
  -> TVar (Maybe MenuAction)
  -> TVar (Maybe String)
  -> TVar [ExplosionEffect]
  -> TVar Viewport
  -> GameScreen
  -> IO Picture
renderGame assets screenVar playerIdVar joinCodeVar hoverVar messageVar explosionEffectsVar viewportVar _ = do
  (screen, maybePlayerId, joinCode, hoverAction, message, explosionEffects, viewport) <- atomically $ do
    currentScreen <- readTVar screenVar
    currentPlayerId <- readTVar playerIdVar
    currentJoinCode <- readTVar joinCodeVar
    currentHover <- readTVar hoverVar
    currentMessage <- readTVar messageVar
    currentExplosionEffects <- readTVar explosionEffectsVar
    currentViewport <- readTVar viewportVar
    pure (currentScreen, currentPlayerId, currentJoinCode, currentHover, currentMessage, currentExplosionEffects, currentViewport)
  pure $ renderInVirtualCanvas viewport $
    renderScreen assets (fromMaybe fallbackPlayerId maybePlayerId) joinCode hoverAction message explosionEffects screen

handleGameEvent
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar (Maybe PlayerId)
  -> TVar String
  -> TVar (Maybe MenuAction)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> TVar Viewport
  -> Event
  -> GameScreen
  -> IO GameScreen
handleGameEvent connection screenVar playerIdVar joinCodeVar hoverVar messageVar audioChannel viewportVar event _ = case event of
  EventResize (newWidth, newHeight) -> atomically $ do
    writeTVar viewportVar $ Viewport newWidth newHeight
    readTVar screenVar
  _ -> do
    (currentScreen, maybePlayerId, viewport) <- atomically $ do
      screen <- readTVar screenVar
      playerId <- readTVar playerIdVar
      currentViewport <- readTVar viewportVar
      pure (screen, playerId, currentViewport)
    let localPlayerId = fromMaybe fallbackPlayerId maybePlayerId
        virtualEvent = eventInVirtualCoordinates viewport event
    case currentScreen of
      ScreenMainMenu -> handleMainMenu connection screenVar joinCodeVar hoverVar messageVar audioChannel virtualEvent
      ScreenCreateRoom -> handleCreateRoom screenVar virtualEvent
      ScreenJoinRoom -> handleJoinRoom connection screenVar joinCodeVar messageVar audioChannel virtualEvent
      ScreenLobby room -> handleLobby connection screenVar localPlayerId messageVar audioChannel room virtualEvent
      ScreenMatch world -> handleMatch connection screenVar localPlayerId messageVar audioChannel world virtualEvent
      ScreenResults _ -> handleResults connection screenVar messageVar audioChannel virtualEvent

-- | All downstream UI hitboxes and camera aim use virtual coordinates, so a
-- | resize/maximize operation cannot desynchronise visuals and input.
eventInVirtualCoordinates :: Viewport -> Event -> Event
eventInVirtualCoordinates viewport event = case event of
  EventKey key keyState modifiers mousePosition ->
    EventKey key keyState modifiers $ windowToVirtual viewport mousePosition
  EventMotion mousePosition -> EventMotion $ windowToVirtual viewport mousePosition
  EventResize size -> EventResize size

handleMainMenu
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar String
  -> TVar (Maybe MenuAction)
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> Event
  -> IO GameScreen
handleMainMenu connection screenVar joinCodeVar hoverVar messageVar audioChannel event = case event of
  EventMotion mousePosition -> do
    let hoverAction = mainMenuActionAt mousePosition
    atomically $ writeTVar hoverVar hoverAction
    pure ScreenMainMenu
  EventKey (MouseButton LeftButton) Down _ mousePosition -> case mainMenuActionAt mousePosition of
    Just MenuCreateRoom -> beginCreateRoom connection screenVar messageVar audioChannel
    Just MenuJoinRoom -> beginJoinRoom screenVar joinCodeVar messageVar audioChannel
    Just MenuExit -> do
      enqueueAudio audioChannel $ PlaySound SfxUiClick
      exitSuccess
    Nothing -> pure ScreenMainMenu
  EventKey (SpecialKey KeyEnter) Down _ _ -> beginCreateRoom connection screenVar messageVar audioChannel
  _ -> pure ScreenMainMenu

handleCreateRoom :: TVar GameScreen -> Event -> IO GameScreen
handleCreateRoom screenVar event = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> setScreen screenVar ScreenMainMenu
  _ -> pure ScreenCreateRoom

beginCreateRoom
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO GameScreen
beginCreateRoom connection screenVar messageVar audioChannel = case connection of
  Nothing -> showConnectionError screenVar messageVar
  Just client -> do
    atomically $ do
      writeTVar screenVar ScreenCreateRoom
      writeTVar messageVar Nothing
    enqueueAudio audioChannel $ PlaySound SfxUiClick
    sendCommand client $ CmdCreateRoom "Player" 3
    pure ScreenCreateRoom

beginJoinRoom
  :: TVar GameScreen
  -> TVar String
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO GameScreen
beginJoinRoom screenVar joinCodeVar messageVar audioChannel = do
  atomically $ do
    writeTVar screenVar ScreenJoinRoom
    writeTVar joinCodeVar ""
    writeTVar messageVar Nothing
  enqueueAudio audioChannel $ PlaySound SfxUiClick
  pure ScreenJoinRoom

handleJoinRoom
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar String
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> Event
  -> IO GameScreen
handleJoinRoom connection screenVar joinCodeVar messageVar audioChannel event = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> setScreen screenVar ScreenMainMenu
  EventKey (MouseButton LeftButton) Down _ mousePosition
    | isJoinCodeInput mousePosition -> do
        enqueueAudio audioChannel $ PlaySound SfxUiClick
        pure ScreenJoinRoom
  EventKey (SpecialKey KeyBackspace) Down _ _ -> do
    atomically $ modifyTVar' joinCodeVar dropLastCharacter
    pure ScreenJoinRoom
  EventKey (SpecialKey KeyEnter) Down _ _ -> do
    roomCodeText <- atomically $ readTVar joinCodeVar
    if null roomCodeText
      then do
        atomically $ writeTVar messageVar $ Just "Enter a room code first."
        pure ScreenJoinRoom
      else case connection of
        Nothing -> showConnectionError screenVar messageVar
        Just client -> do
          atomically $ writeTVar messageVar Nothing
          enqueueAudio audioChannel $ PlaySound SfxUiClick
          sendCommand client $ CmdJoinRoom (RoomCode roomCodeText) "Player"
          pure ScreenJoinRoom
  EventKey (Char typedCharacter) Down _ _
    | isAlphaNum typedCharacter -> do
        atomically $ modifyTVar' joinCodeVar $ appendRoomCodeCharacter typedCharacter
        pure ScreenJoinRoom
  _ -> pure ScreenJoinRoom

handleLobby
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> PlayerId
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> GameRoom
  -> Event
  -> IO GameScreen
handleLobby connection screenVar localPlayerId messageVar audioChannel room event = case event of
  EventKey (SpecialKey KeyEsc) Down _ _ -> leaveRoom connection screenVar messageVar audioChannel
  EventKey (MouseButton LeftButton) Down _ mousePosition -> case lobbyActionAt localPlayerId room mousePosition of
    Nothing -> pure $ ScreenLobby room
    Just action -> submitLobbyAction connection screenVar localPlayerId messageVar audioChannel room action
  _ -> pure $ ScreenLobby room

submitLobbyAction
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> PlayerId
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> GameRoom
  -> LobbyAction
  -> IO GameScreen
submitLobbyAction connection screenVar localPlayerId messageVar audioChannel room action = case action of
  LobbyLeave -> leaveRoom connection screenVar messageVar audioChannel
  _ -> case connection of
    Nothing -> showConnectionError screenVar messageVar
    Just client -> do
      let command = lobbyCommand localPlayerId room action
      atomically $ writeTVar messageVar Nothing
      enqueueAudio audioChannel $ PlaySound SfxUiClick
      sendCommand client command
      pure $ ScreenLobby room

lobbyCommand :: PlayerId -> GameRoom -> LobbyAction -> ClientCommand
lobbyCommand localPlayerId room action = case action of
  LobbySelectTank tankType -> CmdSelectTank tankType currentColor
  LobbySelectColor tankColor -> CmdSelectTank currentTank tankColor
  LobbyToggleReady -> CmdSetReady $ not currentReady
  LobbySelectMap mapType -> CmdSelectMap mapType
  LobbyStartMatch -> CmdStartMatch
  LobbyLeave -> CmdLeaveRoom
  where
    localRoomPlayer = Map.lookup localPlayerId (roomPlayers room)
    currentTank = fromMaybe TankScout $ localRoomPlayer >>= roomPlayerTankType
    currentColor = fromMaybe TankRed $ localRoomPlayer >>= roomPlayerTankColor
    currentReady = maybe False roomPlayerReady localRoomPlayer

leaveRoom
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> IO GameScreen
leaveRoom connection screenVar messageVar audioChannel = do
  maybe (pure ()) (`sendCommand` CmdLeaveRoom) connection
  atomically $ do
    writeTVar screenVar ScreenMainMenu
    writeTVar messageVar Nothing
  enqueueAudio audioChannel $ PlaySound SfxUiClick
  pure ScreenMainMenu

handleMatch
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> PlayerId
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> GameWorld
  -> Event
  -> IO GameScreen
handleMatch connection screenVar localPlayerId messageVar audioChannel world event = case commandForEvent localPlayerId world event of
  Nothing -> pure $ ScreenMatch world
  Just command -> case connection of
    Nothing -> showConnectionError screenVar messageVar
    Just client -> do
      let previewWorld = previewTurretAim localPlayerId command world
      atomically $ do
        writeTVar screenVar $ ScreenMatch previewWorld
        writeTVar messageVar Nothing
      playCommandEffect audioChannel command
      sendCommand client command
      pure $ ScreenMatch previewWorld

handleResults
  :: Maybe ClientConnection
  -> TVar GameScreen
  -> TVar (Maybe String)
  -> TChan AudioCommand
  -> Event
  -> IO GameScreen
handleResults connection screenVar messageVar audioChannel event = case event of
  EventKey (SpecialKey KeyEnter) Down _ _ -> case connection of
    Nothing -> showConnectionError screenVar messageVar
    Just client -> do
      sendCommand client CmdPlayAgain
      enqueueAudio audioChannel $ PlaySound SfxUiClick
      atomically $ readTVar screenVar
  EventKey (SpecialKey KeyEsc) Down _ _ -> leaveRoom connection screenVar messageVar audioChannel
  _ -> atomically (readTVar screenVar)

commandForEvent :: PlayerId -> GameWorld -> Event -> Maybe ClientCommand
commandForEvent localPlayerId world event = case event of
  EventKey (Char 'w') Down _ _ -> movementCommand DirUp
  EventKey (Char 's') Down _ _ -> movementCommand DirDown
  EventKey (Char 'a') Down _ _ -> movementCommand DirLeft
  EventKey (Char 'd') Down _ _ -> movementCommand DirRight
  EventKey (Char 'b') Down _ _ -> Just $ CmdInput $ InputState 0 Nothing Nothing False True
  EventMotion mousePosition -> aimCommand localPlayerId world mousePosition False
  EventKey (MouseButton LeftButton) Down _ mousePosition -> aimCommand localPlayerId world mousePosition True
  _ -> Nothing
  where
    movementCommand direction = Just $ CmdInput $ InputState 0 (Just direction) Nothing False False

aimCommand :: PlayerId -> GameWorld -> Pos -> Bool -> Maybe ClientCommand
aimCommand localPlayerId world mousePosition shootPressed = do
  localTank <- Map.lookup localPlayerId (worldPlayers world)
  let mouseWorldPosition = screenToWorld (playerPosition localTank) cameraZoom mousePosition
      turretAngle = aimAngleTo (playerPosition localTank) mouseWorldPosition
  pure $ CmdInput $ InputState 0 Nothing (Just turretAngle) shootPressed False

previewTurretAim :: PlayerId -> ClientCommand -> GameWorld -> GameWorld
previewTurretAim localPlayerId command world = case command of
  CmdInput inputState -> case inputAimAngle inputState of
    Nothing -> world
    Just angle -> world
      { worldPlayers = Map.adjust setTurret localPlayerId (worldPlayers world) }
      where
        setTurret player = player { playerTurretAngle = angle }
  _ -> world

playCommandEffect :: TChan AudioCommand -> ClientCommand -> IO ()
playCommandEffect audioChannel command = case command of
  CmdInput inputState
    | inputShootPressed inputState -> enqueueAudio audioChannel $ PlaySound SfxShoot
  _ -> pure ()

updateGame
  :: TVar GameScreen
  -> TVar [ExplosionEffect]
  -> TVar (Set.Set PlayerId)
  -> Float
  -> GameScreen
  -> IO GameScreen
updateGame screenVar explosionEffectsVar explodedPlayersVar elapsed _ = atomically $ do
  screen <- readTVar screenVar
  updateDeathExplosions elapsed screen explosionEffectsVar explodedPlayersVar
  pure screen

-- | Player status is authoritative.  This only creates a visual effect once
-- | per dead player and advances the already-created effects between frames.
updateDeathExplosions
  :: Float
  -> GameScreen
  -> TVar [ExplosionEffect]
  -> TVar (Set.Set PlayerId)
  -> STM ()
updateDeathExplosions elapsed screen explosionEffectsVar explodedPlayersVar = case screen of
  ScreenMatch world -> do
    existingEffects <- readTVar explosionEffectsVar
    explodedPlayers <- readTVar explodedPlayersVar
    let deadPlayers = filter ((== PlayerDead) . playerStatus) $ Map.elems (worldPlayers world)
        newEffects =
          [ ExplosionEffect (playerId player) (playerPosition player) 0
          | player <- deadPlayers
          , Set.notMember (playerId player) explodedPlayers
          ]
        deadPlayerIds = Set.fromList $ map playerId deadPlayers
    writeTVar explosionEffectsVar $ advanceExplosionEffects elapsed (existingEffects ++ newEffects)
    writeTVar explodedPlayersVar $ Set.union explodedPlayers deadPlayerIds
  _ -> do
    writeTVar explosionEffectsVar []
    writeTVar explodedPlayersVar Set.empty

showConnectionError :: TVar GameScreen -> TVar (Maybe String) -> IO GameScreen
showConnectionError screenVar messageVar = do
  screen <- atomically $ readTVar screenVar
  atomically $ writeTVar messageVar $ Just "No server connection. Run game-server-exe first."
  pure screen

setScreen :: TVar GameScreen -> GameScreen -> IO GameScreen
setScreen screenVar screen = do
  atomically $ writeTVar screenVar screen
  pure screen

appendRoomCodeCharacter :: Char -> String -> String
appendRoomCodeCharacter typedCharacter roomCode = take 6 $ roomCode ++ [toUpper typedCharacter]

dropLastCharacter :: String -> String
dropLastCharacter roomCode = case reverse roomCode of
  [] -> []
  _ : remainingCharacters -> reverse remainingCharacters

main :: IO ()
main = do
  assets <- loadAssets
  audioChannel <- initSoundSystem
  enqueueAudio audioChannel $ PlayMusic MusicMenu
  screenVar <- newTVarIO ScreenMainMenu
  playerIdVar <- newTVarIO Nothing
  joinCodeVar <- newTVarIO ""
  hoverVar <- newTVarIO Nothing
  messageVar <- newTVarIO Nothing
  explosionEffectsVar <- newTVarIO []
  explodedPlayersVar <- newTVarIO Set.empty
  viewportVar <- newTVarIO initialViewport
  connection <- startNetworkClient "127.0.0.1" "3000" screenVar playerIdVar messageVar audioChannel
  let shutdown = maybe (pure ()) closeConnection connection
  playIO
    window
    bgColor
    60
    ScreenMainMenu
    (renderGame assets screenVar playerIdVar joinCodeVar hoverVar messageVar explosionEffectsVar viewportVar)
    (handleGameEvent connection screenVar playerIdVar joinCodeVar hoverVar messageVar audioChannel viewportVar)
    (updateGame screenVar explosionEffectsVar explodedPlayersVar)
    `finally` shutdown
