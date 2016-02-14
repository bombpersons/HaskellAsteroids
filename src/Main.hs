module Main where
import           Control.Monad
import qualified Data.List              as List
import qualified Data.Map               as Map
import           Data.Maybe
import           SFML.Graphics
import           SFML.Window
import           System.Random

import           Utils
import           World

import           Paths_HaskellAsteroids

makeAsteroid :: Texture -> IO Entity
makeAsteroid t = do
  -- Make it go in a random direction.
  gen <- newStdGen
  let (vx, gen1) = randomR (-1, 1) gen :: (Float, StdGen)
      (vy, gen2) = randomR (-1, 1) gen1 :: (Float, StdGen)
      dir = vecNormalise (Vec2f vx vy)

  let (speed, gen3) = randomR (1, 100) gen2 :: (Float, StdGen)
      (rotSpeed, gen4) = randomR (-pi, pi) gen3 :: (Float, StdGen)

  return $ emptyEntity {eVelocity = multVec dir speed, eRotationSpeed = rotSpeed, eTexture = Just t}

makeWorld :: RenderWindow -> IO World
makeWorld wnd = do
  -- Load textures
  asteroidPath <- getDataFileName "data/asteroid.png"
  playerPath <- getDataFileName "data/player.png"
  asteroidTex <- err $ textureFromFile asteroidPath Nothing
  playerTex <- err $ textureFromFile playerPath Nothing

  -- Make a bunch of asteroids.
  asteroids <- mapM (\dummy -> makeAsteroid asteroidTex) [1..10]
  asteroidSize <- textureSize asteroidTex
  let (Vec2f worldExtend _) = toVec2f asteroidSize

  -- Make a single player.
  let playerEnt = emptyEntity { ePosition = Vec2f 200 200, eRotation = pi/2, eTexture = Just playerTex }

  -- Create the world from these.
  let entities = playerEnt : asteroids
  windowSizeU <- getWindowSize wnd
  let Vec2f x y = toVec2f windowSizeU
      offset = worldExtend * 1.5
      worldRect = FloatRect (0 - offset) (0 - offset) (x + offset) (y + offset)

  let world = World { wWindow = wnd,
                      wOldKeysDown = [],
                      wCurKeysDown = [],
                      wWorldRect = worldRect,
                      wPlayer = playerEnt,
                      wPlayerShootTimer = 0,
                      wAsteroids = asteroids,
                      wBullets = []}
  return world

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
processEvents w@World{wWindow=wnd, wCurKeysDown=curKeys} = do
  evt <- pollEvent wnd
  case evt of
    Just SFEvtClosed -> return Nothing
    Just (SFEvtKeyPressed c _ _ _ _) -> processEvents w {wCurKeysDown = c : curKeys}
    Just (SFEvtKeyReleased c _ _ _ _) -> processEvents w {wCurKeysDown = List.delete c curKeys}
    Nothing -> return (Just w)
    _ -> processEvents w

loop :: Clock -> World -> IO ()
loop clock world@World{wWindow=wnd}= do
  -- Copy over the current keys to the old keys List
  let keyUpdatedWorld = world {wOldKeysDown = wCurKeysDown world}

  -- Poll for events
  polledWorld <- processEvents keyUpdatedWorld
  case polledWorld of
    Nothing -> return ()
    Just validWorld -> do
      -- Clear the window
      clearRenderWindow wnd $ Color 0 0 0 255

      -- Get delta time
      dtMicro <- restartClock clock
      let dt = asSeconds dtMicro

      -- Update world
      let updatedWorld = updateWorld validWorld dt
      renderWorld updatedWorld

      -- Show the window.
      display wnd

      -- Loop
      loop clock updatedWorld
