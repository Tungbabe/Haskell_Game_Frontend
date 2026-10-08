module Sound
  ( initSoundSystem
  , enqueueAudio
  , playAudioCommand
  , playSound
  , soundFileFor
  , musicFileFor
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import System.Directory (doesFileExist)
import System.Process
  ( StdStream(NoStream)
  , createProcess
  , shell
  , std_err
  , std_out
  )
import Types

-- | Create the command queue and process audio away from the Gloss thread.
initSoundSystem :: IO (TChan AudioCommand)
initSoundSystem = do
  audioChannel <- newTChanIO
  void $ forkIO $ audioWorker audioChannel
  pure audioChannel

-- | Adding a command is STM-only and never waits for audio playback.
enqueueAudio :: TChan AudioCommand -> AudioCommand -> IO ()
enqueueAudio audioChannel command =
  atomically $ writeTChan audioChannel command

audioWorker :: TChan AudioCommand -> IO ()
audioWorker audioChannel = forever $ do
  command <- atomically $ readTChan audioChannel
  playAudioCommand command

playAudioCommand :: AudioCommand -> IO ()
playAudioCommand command = case command of
  PlayMusic track -> playFile $ musicFileFor track
  StopMusic -> pure ()
  PlaySound effect -> playSound effect

playSound :: SoundEffect -> IO ()
playSound = playFile . soundFileFor

-- | Windows' SoundPlayer is launched by the background audio worker, never by
-- | the Gloss event or update callbacks.
playFile :: FilePath -> IO ()
playFile file = do
  exists <- doesFileExist file
  when exists $ do
    let command =
          "powershell -NoProfile -Command \"(New-Object Media.SoundPlayer '"
            ++ file
            ++ "').PlaySync()\""
    void $ createProcess (shell command) { std_out = NoStream, std_err = NoStream }

musicFileFor :: MusicTrack -> FilePath
musicFileFor track = case track of
  MusicMenu -> "assets/sounds/menu_bgm.wav"
  MusicMatch -> "assets/sounds/bgm.wav"

soundFileFor :: SoundEffect -> FilePath
soundFileFor effect = case effect of
  SfxUiClick -> "assets/sounds/ui_click.wav"
  SfxShoot -> "assets/sounds/shoot.wav"
  SfxItemPickup ItemHeart -> "assets/sounds/item_heart.wav"
  SfxItemPickup ItemShield -> "assets/sounds/item_shield.wav"
  SfxItemPickup ItemBomb -> "assets/sounds/item_bomb.wav"
  SfxBombExplosion -> "assets/sounds/bomb.wav"
  SfxKill -> "assets/sounds/kill.wav"
  SfxDeath -> "assets/sounds/death.wav"
  SfxVictory -> "assets/sounds/victory.wav"
  SfxDefeat -> "assets/sounds/defeat.wav"
