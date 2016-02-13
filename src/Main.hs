module Main where
import           Control.Monad
import qualified Data.Map               as Map
import           SFML.Graphics
import           SFML.Window
import           System.Random

import           Paths_HaskellAsteroids

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

data Entity = Entity {
  eSprite        :: Maybe Sprite,
  ePosition      :: Vec2f,
  eRotation      :: Float,
  eVelocity      :: Vec2f,
  eRotationSpeed :: Float,
  eRender        :: World -> IO (),
  eUpdate        :: World -> Float -> Entity
}

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
drawSpriteAtPos e world@World{wWindow=wnd} =
  case eSprite e of Nothing -> return ()
                    Just spr -> do
                      texRect <- getTextureRect spr
                      let width = fromIntegral (iwidth texRect)
                          height = fromIntegral (iheight texRect)

                      setOrigin spr (Vec2f (width / 2) (height / 2))
                      setPosition spr (ePosition e)
                      setRotation spr (eRotation e)
                      drawSprite wnd spr Nothing

moveAtVelocity :: Entity -> World -> Float -> Entity
moveAtVelocity e@Entity{eVelocity=Vec2f vx vy, ePosition=Vec2f px py, eRotation=r, eRotationSpeed=rs} w@World{wWindowSize=Vec2f wx wy} dt =
  let possiblePx = px + vx * dt
      possiblePy = py + vy * dt
      newPx
        | possiblePx > wx = possiblePx - wx
        | possiblePx < 0 = wx - possiblePx
        | otherwise = possiblePx
      newPy
        | possiblePy > wy = possiblePy - wy
        | possiblePy < 0 = wy - possiblePy
        | otherwise = possiblePy
  in e { ePosition = Vec2f newPx newPy, eRotation = r + rs * dt}

asteroid ent = ent { eRender = drawSpriteAtPos ent,
                     eUpdate = \world delta -> asteroid (moveAtVelocity ent world delta) }

player ent = ent { eRender = drawSpriteAtPos ent,
                   eUpdate = \world delta -> player (moveAtVelocity ent world delta) }

data World = World { wWindow     :: RenderWindow,
                     wWindowSize :: Vec2f,
                     wEntities   :: [Entity] }

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
      dir = (Vec2f vx vy)

  let (speed, gen3) = randomR (1, 100) gen2 :: (Float, StdGen)
      (rotSpeed, gen4) = randomR (-100, 100) gen3 :: (Float, StdGen)

  return $ asteroid (emptyEntity {eVelocity = multVec dir speed, eRotationSpeed = rotSpeed, eSprite = Just spr})

makeWorld :: RenderWindow -> Texture -> Texture -> IO World
makeWorld wnd playerTex asteroidTex = do
  -- Make a bunch of asteroids.
  asteroids <- mapM (\dummy -> makeAsteroid asteroidTex) [1..10]

  -- Make a single player.
  playerSprite <- err createSprite
  setTexture playerSprite playerTex True
  let playerEnt = player emptyEntity { eSprite = Just playerSprite }

  -- Create the world from these.
  let entities = playerEnt : asteroids
  windowSizeU <- getWindowSize wnd
  let windowSize = toVec2f windowSizeU

  let world = World { wWindow = wnd, wWindowSize = windowSize, wEntities = entities }
  return world

main :: IO ()
main = do
  -- Make a window.
  let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 640 480 32) "Haskell Asteroids" [SFDefaultStyle] ctxSettings

  -- Load textures
  asteroidPath <- getDataFileName "data/asteroid.gif"
  playerPath <- getDataFileName "data/player.jpg"
  asteroidTex <- err $ textureFromFile asteroidPath Nothing
  playerTex <- err $ textureFromFile playerPath Nothing

  -- Make the world
  world <- makeWorld wnd playerTex asteroidTex

  -- Create a Clock
  clock <- createClock

  -- Loop
  loop clock world
  destroy wnd

loop :: Clock -> World -> IO ()
loop clock world@World{ wWindow = wnd }= do
  clearRenderWindow wnd $ Color 100 100 100 255

  -- Get delta time
  dtMicro <- restartClock clock
  let dt = asSeconds dtMicro

  -- Update world
  let updatedWorld = updateWorld world dt
  renderWorld updatedWorld

  -- Show the window.
  display wnd

  evt <- pollEvent wnd
  case evt of
    Just SFEvtClosed -> return ()
    _ -> loop clock updatedWorld
