module Render.UI
  ( MenuAction(..), LobbyAction(..), renderUI, renderMainMenu, renderCreateRoom
  , renderJoinRoom, renderLobby, renderResults, renderStatusMessage
  , mainMenuActionAt, lobbyActionAt, isJoinCodeInput
  ) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss
import Types

data MenuAction = MenuCreateRoom | MenuJoinRoom | MenuExit deriving (Show, Eq)
data LobbyAction
  = LobbySelectTank TankType | LobbySelectColor TankColor | LobbyToggleReady
  | LobbySelectMap MapType | LobbyStartMatch | LobbyLeave
  deriving (Show, Eq)

renderMainMenu :: Assets -> Maybe MenuAction -> Picture
renderMainMenu assets hover = Pictures
  [ renderMenuBackground assets
  , menuButton assets hover MenuCreateRoom createRoomButtonCenter
  , menuButton assets hover MenuJoinRoom joinRoomButtonCenter
  , menuButton assets hover MenuExit exitButtonCenter
  ]

renderCreateRoom :: Assets -> Picture
renderCreateRoom assets = Pictures
  [ renderDimmedMenuBackground assets, panel (0, 15) 520 260
  , centered 0 85 0.32 white "CREATING ROOM"
  , centered 0 25 0.20 (greyN 0.85) "Waiting for server response"
  , centered 0 (-80) 0.17 white "Esc: Back to menu"
  ]

renderJoinRoom :: Assets -> String -> Picture
renderJoinRoom assets roomCode = Pictures
  [ renderDimmedMenuBackground assets, panel (0, 10) 560 320
  , centered 0 125 0.32 white "JOIN ROOM"
  , centered 0 75 0.19 white "Enter room code"
  , joinCodeInput roomCode
  , centered 0 (-105) 0.16 (greyN 0.82) "Enter: Join    Esc: Back"
  ]

joinCodeInput :: String -> Picture
joinCodeInput roomCode = Pictures
  [ Translate 0 5 $ Color (makeColorI 23 53 78 255) $ rectangleSolid joinInputWidth joinInputHeight
  , Translate 0 5 $ Color (makeColorI 94 176 218 255) $ rectangleWire joinInputWidth joinInputHeight
  , centered 0 (-7) 0.30 yellow displayCode
  ]
  where displayCode | null roomCode = "_" | otherwise = roomCode

menuButton :: Assets -> Maybe MenuAction -> MenuAction -> Pos -> Picture
menuButton assets hover action (x, y) = Pictures
  [ Translate x y $ Color border $ rectangleSolid (menuButtonWidth + borderSize) (menuButtonHeight + borderSize)
  , Translate x y $ Scale imageScale imageScale image
  ]
  where
    active = hover == Just action
    imageScale | active = menuButtonScale * 1.07 | otherwise = menuButtonScale
    borderSize | active = 16 | otherwise = 8
    border | active = makeColorI 92 206 248 255 | otherwise = makeColorI 18 28 42 235
    image = case action of
      MenuCreateRoom -> assetsCreateRoomButton assets
      MenuJoinRoom -> assetsJoinRoomButton assets
      MenuExit -> assetsExitButton assets

mainMenuActionAt :: Pos -> Maybe MenuAction
mainMenuActionAt pos
  | inRect createRoomButtonCenter menuButtonWidth menuButtonHeight pos = Just MenuCreateRoom
  | inRect joinRoomButtonCenter menuButtonWidth menuButtonHeight pos = Just MenuJoinRoom
  | inRect exitButtonCenter menuButtonWidth menuButtonHeight pos = Just MenuExit
  | otherwise = Nothing

isJoinCodeInput :: Pos -> Bool
isJoinCodeInput = inRect (0, 5) joinInputWidth joinInputHeight

renderLobby :: Assets -> PlayerId -> GameRoom -> Picture
renderLobby assets localId room = Pictures
  [ renderDimmedMenuBackground assets
  , panel (0, 300) 1180 92
  , centered 0 320 0.31 white "LOBBY"
  , centered 0 278 0.20 yellow $ "ROOM CODE  " ++ unRoomCode (roomCode room)
  , panel playerPanelCenter playerPanelWidth playerPanelHeight
  , centered playerPanelX 225 0.20 white "PLAYERS"
  , renderRoomPlayers room
  , lobbyButton readyButtonCenter readyLabel
  , lobbyButton leaveButtonCenter "LEAVE"
  , panel selectionPanelCenter selectionPanelWidth selectionPanelHeight
  , textAt selectionPanelLeft 225 0.17 white "TANK SELECTION"
  , tankChoices assets localPlayer
  , textAt selectionPanelLeft 80 0.17 white "COLOR SELECTION"
  , colorChoices localPlayer
  , hostControls assets localId room
  ]
  where
    localPlayer = Map.lookup localId (roomPlayers room)
    readyLabel = case localPlayer of
      Just player | roomPlayerReady player -> "NOT READY"
      _ -> "READY"

renderRoomPlayers :: GameRoom -> Picture
renderRoomPlayers room = Pictures $ zipWith entry [0 :: Int ..] (Map.elems $ roomPlayers room)
  where
    entry index player = Pictures
      [ textAt playerTextX y 0.17 playerColor $ roomPlayerName player ++ hostSuffix
      , textAt playerTextX (y - 24) 0.13 (greyN 0.87) status
      , Translate playerMarkerX (y + 3) $ Color playerColor $ circleSolid 5
      ]
      where
        y = 178 - fromIntegral index * playerEntrySpacing
        hostSuffix | roomPlayerId player == roomHostId room = "  HOST" | otherwise = ""
        tank = case (roomPlayerTankType player, roomPlayerTankColor player) of
          (Just tankType, Just tankColor) -> show tankType ++ " / " ++ show tankColor
          _ -> "Choosing tank and color"
        state | roomPlayerReady player = "READY" | otherwise = "NOT READY"
        status = tank ++ "  -  " ++ state
        playerColor | roomPlayerReady player = makeColorI 112 230 142 255 | otherwise = white

tankChoices :: Assets -> Maybe RoomPlayer -> Picture
tankChoices assets maybePlayer = Pictures
  [ tankCard TankScout tankChoiceScout
  , tankCard TankHeavy tankChoiceHeavy
  , tankCard TankArtillery tankChoiceArtillery
  ]
  where
    color = maybe TankRed (maybe TankRed id . roomPlayerTankColor) maybePlayer
    tankCard tankType (x, y) = Pictures
      [ Translate x y $ Color (choiceBorder selected) $ rectangleSolid (tankChoiceWidth + 8) (tankChoiceHeight + 8)
      , Translate x y $ Color (choiceBackground selected) $ rectangleSolid tankChoiceWidth tankChoiceHeight
      , Translate x (y + 10) $ tankPreview assets tankType color
      , centered x (y - 42) 0.14 white (tankLabel tankType)
      ]
      where selected = maybe False ((== Just tankType) . roomPlayerTankType) maybePlayer

tankPreview :: Assets -> TankType -> TankColor -> Picture
tankPreview assets tankType tankColor = Pictures
  [ Scale tankPreviewScale tankPreviewScale $ Map.findWithDefault blank (tankType, tankColor, DirUp) (assetsTankBodySprites assets)
  , Scale tankPreviewScale tankPreviewScale $ Map.findWithDefault blank (tankType, tankColor) (assetsTankTurretSprites assets)
  ]

colorChoices :: Maybe RoomPlayer -> Picture
colorChoices maybePlayer = Pictures
  [ colorCard TankRed "RED" colorChoiceRed
  , colorCard TankBlue "BLUE" colorChoiceBlue
  , colorCard TankGreen "GREEN" colorChoiceGreen
  ]
  where
    colorCard tankColor label (x, y) = Pictures
      [ Translate x y $ Color (choiceBorder selected) $ rectangleSolid (colorChoiceWidth + 8) (colorChoiceHeight + 8)
      , Translate x y $ Color (colorFill tankColor) $ rectangleSolid colorChoiceWidth colorChoiceHeight
      , centered x (y - 8) 0.15 white label
      ]
      where selected = maybe False ((== Just tankColor) . roomPlayerTankColor) maybePlayer

hostControls :: Assets -> PlayerId -> GameRoom -> Picture
hostControls assets localId room
  | roomHostId room /= localId = Pictures
      [ centered selectionPanelX (-92) 0.16 (greyN 0.82) "Waiting for host to choose a map"
      , centered selectionPanelX (-125) 0.14 (greyN 0.70) "Select tank, color, and Ready while you wait"
      ]
  | otherwise = Pictures
      [ textAt selectionPanelLeft (-72) 0.17 white "HOST MAP SELECTION"
      , mapCard MapDepot mapChoiceDepot
      , mapCard MapForest mapChoiceForest
      , mapCard MapRuins mapChoiceRuins
      , lobbyButton startButtonCenter "START MATCH"
      ]
  where
    mapCard mapType (x, y) = Pictures
      [ Translate x y $ Color (choiceBorder selected) $ rectangleSolid (mapChoiceWidth + 8) (mapChoiceHeight + 8)
      , Translate x y $ Color (choiceBackground selected) $ rectangleSolid mapChoiceWidth mapChoiceHeight
      , Translate x y $ Scale mapButtonScale mapButtonScale image
      ]
      where
        selected = roomSelectedMap room == Just mapType
        image = Map.findWithDefault blank mapType (assetsMapButtonSprites assets)

lobbyButton :: Pos -> String -> Picture
lobbyButton (x, y) label = Pictures
  [ Translate x y $ Color (makeColorI 155 105 42 255) $ rectangleSolid lobbyButtonWidth lobbyButtonHeight
  , Translate x y $ Color (makeColorI 239 192 91 255) $ rectangleWire lobbyButtonWidth lobbyButtonHeight
  , centered x (y - 9) 0.16 white label
  ]

lobbyActionAt :: PlayerId -> GameRoom -> Pos -> Maybe LobbyAction
lobbyActionAt localId room pos
  | inRect tankChoiceScout tankChoiceWidth tankChoiceHeight pos = Just $ LobbySelectTank TankScout
  | inRect tankChoiceHeavy tankChoiceWidth tankChoiceHeight pos = Just $ LobbySelectTank TankHeavy
  | inRect tankChoiceArtillery tankChoiceWidth tankChoiceHeight pos = Just $ LobbySelectTank TankArtillery
  | inRect colorChoiceRed colorChoiceWidth colorChoiceHeight pos = Just $ LobbySelectColor TankRed
  | inRect colorChoiceBlue colorChoiceWidth colorChoiceHeight pos = Just $ LobbySelectColor TankBlue
  | inRect colorChoiceGreen colorChoiceWidth colorChoiceHeight pos = Just $ LobbySelectColor TankGreen
  | inRect readyButtonCenter lobbyButtonWidth lobbyButtonHeight pos = Just LobbyToggleReady
  | inRect leaveButtonCenter lobbyButtonWidth lobbyButtonHeight pos = Just LobbyLeave
  | isHost && inRect mapChoiceDepot mapChoiceWidth mapChoiceHeight pos = Just $ LobbySelectMap MapDepot
  | isHost && inRect mapChoiceForest mapChoiceWidth mapChoiceHeight pos = Just $ LobbySelectMap MapForest
  | isHost && inRect mapChoiceRuins mapChoiceWidth mapChoiceHeight pos = Just $ LobbySelectMap MapRuins
  | isHost && inRect startButtonCenter lobbyButtonWidth lobbyButtonHeight pos = Just LobbyStartMatch
  | otherwise = Nothing
  where isHost = roomHostId room == localId

renderResults :: Maybe PlayerId -> MatchResult -> Picture
renderResults localId result = Pictures
  [ panel (0, 15) 880 560
  , centered 0 245 0.38 titleColor title
  , centered 0 190 0.20 white winner
  , Pictures $ zipWith rank [0 :: Int ..] (resultRanking result)
  , centered 0 (-225) 0.16 white "Play Again: Enter        Main Menu: Esc"
  ]
  where
    victory = localId == resultWinnerId result
    title | victory = "VICTORY" | otherwise = "DEFEAT"
    titleColor | victory = green | otherwise = red
    winner = maybe "No winner" (\winnerId -> "Winner: " ++ show winnerId) (resultWinnerId result)
    rank index entry = textAt (-335) (130 - fromIntegral index * 48) 0.16 white $
      show (rankingPlace entry) ++ ". " ++ rankingPlayerName entry
        ++ "  " ++ show (rankingTankType entry) ++ " / " ++ show (rankingTankColor entry)
        ++ "  Kills: " ++ show (rankingKills entry) ++ "  Points: " ++ show (rankingScore entry)

renderUI :: Maybe Player -> Picture
renderUI maybePlayer = case maybePlayer of
  Nothing -> blank
  Just player -> Pictures
    [ textAt (-600) 320 0.18 white $ playerName player
    , healthBar player
    , textAt (-600) 230 0.16 white $ "Shield: " ++ shieldLabel player
    , textAt (-600) 195 0.16 white $ "Bombs: " ++ show (playerBombCount player)
    , textAt (-600) 160 0.16 white $ "Kills: " ++ show (playerKills player)
    , textAt (-600) 125 0.16 white $ "Score: " ++ show (playerScore player)
    ]

healthBar :: Player -> Picture
healthBar player = Pictures
  [ Translate healthBarCenterX 274 $ Color (makeColorI 85 25 25 255) $ rectangleSolid healthBarWidth healthBarHeight
  , Translate fillCenter 274 $ Color green $ rectangleSolid fillWidth healthBarHeight
  , textAt (-600) 252 0.15 white $ "HP: " ++ show (playerHealth player) ++ "/" ++ show (playerMaxHealth player)
  ]
  where
    ratio = max 0 $ min 1 $ fromIntegral (playerHealth player) / fromIntegral (max 1 $ playerMaxHealth player)
    fillWidth = healthBarWidth * ratio
    fillCenter = healthBarCenterX - healthBarWidth / 2 + fillWidth / 2

shieldLabel :: Player -> String
shieldLabel player
  | playerShieldRemaining player > 0 = show (fromIntegral (round (playerShieldRemaining player * 10)) / 10 :: Float) ++ "s"
  | otherwise = "off"

renderStatusMessage :: Maybe String -> Picture
renderStatusMessage Nothing = blank
renderStatusMessage (Just message) = Pictures
  [ Translate 0 (-325) $ Color (makeColorI 100 35 35 235) $ rectangleSolid 1100 44
  , centered 0 (-335) 0.16 white message
  ]

panel :: Pos -> Float -> Float -> Picture
panel (x, y) width height = Pictures
  [ Translate x y $ Color (makeColorI 15 23 35 220) $ rectangleSolid width height
  , Translate x y $ Color (makeColorI 92 151 184 210) $ rectangleWire width height
  ]

choiceBackground :: Bool -> Color
choiceBackground selected | selected = makeColorI 43 100 81 255 | otherwise = makeColorI 44 53 68 255

choiceBorder :: Bool -> Color
choiceBorder selected | selected = makeColorI 255 220 82 255 | otherwise = makeColorI 78 115 141 255

colorFill :: TankColor -> Color
colorFill TankRed = makeColorI 155 61 61 255
colorFill TankBlue = makeColorI 55 103 170 255
colorFill TankGreen = makeColorI 56 137 86 255

tankLabel :: TankType -> String
tankLabel TankScout = "SCOUT"
tankLabel TankHeavy = "HEAVY"
tankLabel TankArtillery = "ARTILLERY"

mapLabel :: MapType -> String
mapLabel MapDepot = "DEPOT"
mapLabel MapForest = "FOREST"
mapLabel MapRuins = "RUINS"

inRect :: Pos -> Float -> Float -> Pos -> Bool
inRect (centerX, centerY) width height (pointX, pointY) =
  abs (pointX - centerX) <= width / 2 && abs (pointY - centerY) <= height / 2

textAt :: Float -> Float -> Float -> Color -> String -> Picture
textAt x y scaleValue textColor label = Translate x y $ Scale scaleValue scaleValue $ Color textColor $ Text label

centered :: Float -> Float -> Float -> Color -> String -> Picture
centered x y scaleValue textColor label = textAt (x - textWidth label scaleValue / 2) y scaleValue textColor label

textWidth :: String -> Float -> Float
textWidth label scaleValue = fromIntegral (length label) * 55 * scaleValue

createRoomButtonCenter, joinRoomButtonCenter, exitButtonCenter :: Pos
createRoomButtonCenter = (0, -35)
joinRoomButtonCenter = (0, -140)
exitButtonCenter = (0, -245)

playerPanelCenter, selectionPanelCenter :: Pos
playerPanelCenter = (-460, 10)
selectionPanelCenter = (160, 10)

playerPanelX, selectionPanelX, selectionPanelLeft :: Float
playerPanelX = -460
selectionPanelX = 160
selectionPanelLeft = -250

tankChoiceScout, tankChoiceHeavy, tankChoiceArtillery :: Pos
tankChoiceScout = (-130, 165)
tankChoiceHeavy = (150, 165)
tankChoiceArtillery = (430, 165)

colorChoiceRed, colorChoiceBlue, colorChoiceGreen :: Pos
colorChoiceRed = (-130, 35)
colorChoiceBlue = (150, 35)
colorChoiceGreen = (430, 35)

readyButtonCenter, leaveButtonCenter, startButtonCenter :: Pos
readyButtonCenter = (-460, -160)
leaveButtonCenter = (-460, -215)
startButtonCenter = (150, -202)

mapChoiceDepot, mapChoiceForest, mapChoiceRuins :: Pos
mapChoiceDepot = (-130, -135)
mapChoiceForest = (150, -135)
mapChoiceRuins = (430, -135)

menuButtonWidth, menuButtonHeight, menuButtonScale :: Float
menuButtonWidth = 270
menuButtonHeight = 87
menuButtonScale = 0.128

joinInputWidth, joinInputHeight :: Float
joinInputWidth = 320
joinInputHeight = 65

playerPanelWidth, playerPanelHeight, selectionPanelWidth, selectionPanelHeight :: Float
playerPanelWidth = 300
playerPanelHeight = 500
selectionPanelWidth = 870
selectionPanelHeight = 500

tankChoiceWidth, tankChoiceHeight, colorChoiceWidth, colorChoiceHeight :: Float
tankChoiceWidth = 240
tankChoiceHeight = 108
colorChoiceWidth = 240
colorChoiceHeight = 54

mapChoiceWidth, mapChoiceHeight, mapButtonScale :: Float
mapChoiceWidth = 240
mapChoiceHeight = 76
mapButtonScale = 0.082

lobbyButtonWidth, lobbyButtonHeight :: Float
lobbyButtonWidth = 240
lobbyButtonHeight = 44

tankPreviewScale, healthBarWidth, healthBarHeight, healthBarCenterX :: Float
tankPreviewScale = 0.24
healthBarWidth = 230
healthBarHeight = 18
healthBarCenterX = -485

playerTextX, playerMarkerX, playerEntrySpacing :: Float
playerTextX = -585
playerMarkerX = -600
playerEntrySpacing = 66

renderMenuBackground :: Assets -> Picture
renderMenuBackground assets = Scale 0.70 0.70 $ assetsMenuBackground assets

renderDimmedMenuBackground :: Assets -> Picture
renderDimmedMenuBackground assets = Color (makeColor 0.58 0.58 0.64 0.72) $ renderMenuBackground assets

unRoomCode :: RoomCode -> String
unRoomCode (RoomCode code) = code
