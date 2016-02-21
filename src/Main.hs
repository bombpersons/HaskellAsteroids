module Main where
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import qualified Data.List              as List
import qualified Data.Map               as Map
import           Data.Maybe
import           SFML.Graphics
import           SFML.Window
import           System.Random

import           Utils
import           World

import           Paths_HaskellAsteroids

makeWorld :: RenderWindow -> IO World
makeWorld wnd = return $ window .~ Just wnd $ defWorld

main :: IO ()
main = do
  -- Make a window.
  let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 800 600 32) "Hasteroids" [SFDefaultStyle] ctxSettings
  setKeyRepeat wnd False

  -- Make the world
  world <- makeWorld wnd

  -- Create a Clock
  clock <- createClock

  -- Loop
  loop clock world
  destroy wnd

processEvents :: World -> IO (Maybe World)
processEvents w =
  case w ^. window of
    Just wnd -> do
      evt <- pollEvent wnd
      case evt of
        Just SFEvtClosed -> return Nothing
        Just (SFEvtKeyPressed c _ _ _ _) -> processEvents $ curKeysDown %~ (c:) $ w
        Just (SFEvtKeyReleased c _ _ _ _) -> processEvents $ curKeysDown %~ List.delete c $ w
        Nothing -> return (Just w)
        _ -> processEvents w
    Nothing -> return (Just w)

loop :: Clock -> World -> IO ()
loop clock world@World{}= do
  -- Copy over the current keys to the old keys List
  let keysDown = world ^. curKeysDown
  let keyUpdatedWorld = set oldKeysDown keysDown world

  -- Poll for events
  polledWorld <- processEvents keyUpdatedWorld
  case polledWorld of
    Nothing -> return ()
    Just validWorld -> do
      -- Clear the window
      let Just wnd = world^.window
      clearRenderWindow wnd $ Color 0 0 0 255

      -- Get delta time
      dtMicro <- restartClock clock
      let dt = asSeconds dtMicro

      -- Update world
      let updatedWorld = execState (updateWorld dt) validWorld
      renderWorld updatedWorld

      -- Show the window.
      display wnd

      -- Loop
      loop clock updatedWorld
