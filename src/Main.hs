module Main where
import           Control.Monad
import qualified Data.List              as List
import           Data.Maybe
import           SFML.Graphics
import           SFML.Window
import           System.Random

import           Paths_HaskellAsteroids

-- Utility functions
multVec :: Vec2f -> Float -> Vec2f
multVec (Vec2f x y) n = Vec2f (x * n) (y * n)

divVec :: Vec2f -> Float -> Vec2f
divVec v f = multVec v (1/f)

toVec2f :: Vec2u -> Vec2f
toVec2f (Vec2u x y) = Vec2f (fromIntegral x) (fromIntegral y)

vecLength :: Vec2f -> Float
vecLength (Vec2f x y) = sqrt ((x * x) + (y * y))

vecNormalise :: Vec2f -> Vec2f
vecNormalise v = divVec v (vecLength v)

toDegrees :: Float -> Float
toDegrees a = (a / (2 * pi)) * 360

getForwardVecFromRotation :: Float -> Vec2f
getForwardVecFromRotation a = Vec2f (sin (-a)) (cos (-a))

-- Input
worldIsKeyDown :: World -> KeyCode -> Bool
worldIsKeyDown w@World{wCurKeysDown=keys} key = isJust (List.find (==key) keys)

worldIsKeyPressed :: World -> KeyCode -> Bool
worldIsKeyPressed w@World{wCurKeysDown=curKeys, wOldKeysDown=oldKeys} key = isJust (List.find (==key) curKeys) && isNothing (List.find (==key) oldKeys)

worldIsKeyReleased :: World -> KeyCode -> Bool
worldIsKeyReleased w@World{wCurKeysDown=curKeys, wOldKeysDown=oldKeys} key = isNothing (List.find (==key) curKeys) && isJust (List.find (==key) oldKeys)

-- Entity system
data Entity = Entity {
  eSprite        :: Maybe Sprite,
  ePosition      :: Vec2f,
  eRotation      :: Float,
  eVelocity      :: Vec2f,
  eRotationSpeed :: Float,
  eRender        :: World -> IO (),
  eUpdate        :: World -> Float -> Entity
}

data World = World { wWindow      :: RenderWindow,
                     wOldKeysDown :: [KeyCode],
                     wCurKeysDown :: [KeyCode],
                     wWorldRect   :: FloatRect,
                     wEntities    :: [Entity] }

emptyEntity :: Entity
emptyEntity = Entity {
                eSprite = Nothing,
                ePosition = Vec2f 0 0,
                eRotation = 0,
                eVelocity = Vec2f 0 0,
                eRotationSpeed = 0,
                eRender = \world -> return (),
                eUpdate = \world delta -> emptyEntity
}

drawSpriteAtPos :: Entity -> World -> IO ()
drawSpriteAtPos e world@World{wWindow=wnd, wWorldRect=FloatRect wx wy ww wh} =
  case eSprite e of Nothing -> return ()
                    Just spr -> do
                      texRect <- getTextureRect spr
                      let width = fromIntegral (iwidth texRect)
                          height = fromIntegral (iheight texRect)
                      let pos = ePosition e + divVec (Vec2f wx wy) 2

                      setOrigin spr (Vec2f (width / 2) (height / 2))
                      setPosition spr pos
                      setRotation spr (toDegrees (eRotation e))
                      drawSprite wnd spr Nothing

moveAtVelocity :: World -> Float -> Entity -> Entity
moveAtVelocity w@World{wWorldRect=FloatRect wx wy ww wh} dt e@Entity{eVelocity=Vec2f vx vy, ePosition=Vec2f px py, eRotation=r, eRotationSpeed=rs} =
  let possiblePx = (px + vx * dt)
      possiblePy = (py + vy * dt)
      newPx
        | possiblePx > ww = possiblePx - ww
        | possiblePx < 0 = ww - possiblePx
        | otherwise = possiblePx
      newPy
        | possiblePy > wh = possiblePy - wh
        | possiblePy < 0 = wh - possiblePy
        | otherwise = possiblePy
  in e { ePosition = Vec2f newPx newPy, eRotation = r + rs * dt}

moveBackAndForwardKeys :: Float -> World -> Float -> Entity -> Entity
moveBackAndForwardKeys acc w dt e@Entity{eVelocity=v, eRotation=r} = e{eVelocity = newVel}
  where up = if worldIsKeyDown w KeyUp then (-1) else 0
        down = if worldIsKeyDown w KeyDown then 1 else 0
        dir = multVec (getForwardVecFromRotation r) (up + down)
        newVel = v + multVec dir (acc * dt)

turnLeftRightKeys :: Float -> World -> Float -> Entity -> Entity
turnLeftRightKeys acc w dt e@Entity{eRotationSpeed=v} = e{eRotationSpeed = newSpeed}
  where left = if worldIsKeyDown w KeyLeft then (-1) else 0
        right = if worldIsKeyDown w KeyRight then 1 else 0
        dir = left + right
        newSpeed = v + dir * acc * dt

asteroid ent = ent { eRender = drawSpriteAtPos ent,
                     eUpdate = \world delta -> asteroid . moveAtVelocity world delta $ ent }

player acc rotAcc ent = ent { eRender = drawSpriteAtPos ent,
                              eUpdate = \world delta -> player acc rotAcc . moveBackAndForwardKeys acc world delta
                                                                          . turnLeftRightKeys rotAcc world delta
                                                                          . moveAtVelocity world delta $ ent }

updateWorld :: World -> Float -> World
updateWorld world@World{wEntities=ent} dt = world {wEntities = fmap update ent }
  where update e = eUpdate e world dt

renderWorld :: World -> IO ()
renderWorld world@World{wEntities=ent} = mapM_ render ent
  where render e = eRender e world

makeAsteroid :: Texture -> IO Entity
makeAsteroid t = do
  spr <- err createSprite
  setTexture spr t True

  -- Make it go in a random direction.
  gen <- newStdGen
  let (vx, gen1) = randomR (-1, 1) gen :: (Float, StdGen)
      (vy, gen2) = randomR (-1, 1) gen1 :: (Float, StdGen)
      dir = vecNormalise (Vec2f vx vy)

  let (speed, gen3) = randomR (1, 100) gen2 :: (Float, StdGen)
      (rotSpeed, gen4) = randomR (-pi, pi) gen3 :: (Float, StdGen)

  return $ asteroid (emptyEntity {eVelocity = multVec dir speed, eRotationSpeed = rotSpeed, eSprite = Just spr})

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
  playerSprite <- err createSprite
  setTexture playerSprite playerTex True
  let playerEnt = player 500 (2*pi) emptyEntity { ePosition = Vec2f 200 200, eRotation = pi/2, eSprite = Just playerSprite }

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
                      wEntities = entities }
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
      clearRenderWindow wnd $ Color 100 100 100 255

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
