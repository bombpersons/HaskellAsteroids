module Main where
import           Control.Monad
import qualified Data.Map               as Map
import           SFML.Graphics
import           SFML.Window

import           Paths_HaskellAsteroids

multVec :: Vec2f -> Float -> Vec2f
multVec (Vec2f x y) n = Vec2f (x * n) (y * n)

data Entity = Entity {
  eSprite   :: Sprite,
  ePosition :: Vec2f,
  eVelocity :: Vec2f,
  eRender   :: RenderWindow -> IO (),
  eUpdate   :: Float -> Entity
}

drawSpriteAtPos :: Entity -> RenderWindow -> IO ()
drawSpriteAtPos e wnd = do
  setPosition (eSprite e) (ePosition e)
  drawSprite wnd (eSprite e) Nothing

asteroid p v s = let e = Entity { eSprite = s,
                                  ePosition = p,
                                  eVelocity = v,
                                  eRender = drawSpriteAtPos e,
                                  eUpdate = \delta -> asteroid (p + (multVec v delta)) v s }
                 in e

player p v s = let e = Entity { eSprite = s,
                                ePosition = p,
                                eVelocity = v,
                                eRender = drawSpriteAtPos e,
                                eUpdate = \delta -> player p v s }
               in e

type World = [Entity]

updateWorld :: World -> Float -> World
updateWorld w dt = fmap update w
  where update e = eUpdate e dt

renderWorld :: RenderWindow -> World -> IO ()
renderWorld wnd w = mapM_ render w
  where render e = eRender e wnd

makeAsteroid :: Texture -> IO Entity
makeAsteroid t = do
  spr <- err $ createSprite
  setTexture spr t True
  return $ asteroid (Vec2f 0 0) (Vec2f 1 1) spr

makeWorld :: Texture -> Texture -> IO World
makeWorld asteroidTex playerTex = do
  asteroids <- replicateM 10 (makeAsteroid asteroidTex)

  playerSprite <- err $ createSprite
  setTexture playerSprite playerTex True
  let playerEnt = player (Vec2f 0 0) (Vec2f 0 0) playerSprite

  let world = playerEnt : asteroids
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
  world <- makeWorld playerTex asteroidTex

  -- Create a Clock
  clock <- createClock

  -- Loop
  loop wnd clock world
  destroy wnd

loop :: RenderWindow -> Clock -> World -> IO ()
loop wnd clock world = do
  clearRenderWindow wnd $ Color 100 100 100 255

  -- Get delta time
  dtMicro <- restartClock clock
  let dt = asSeconds dtMicro

  -- Update world
  let updatedWorld = updateWorld world dt
  renderWorld wnd updatedWorld

  display wnd

  evt <- pollEvent wnd
  case evt of
    Just SFEvtClosed -> return ()
    _ -> loop wnd clock updatedWorld
