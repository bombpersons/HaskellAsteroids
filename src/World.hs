{-# LANGUAGE TemplateHaskell #-}

module World where
import           Control.Lens
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe
import           SFML.Graphics
import           SFML.Window
import           Utils

type Entity = Int

data Transformable = Transformable {
  _position :: Vec2f,
  _rotation :: Float
}
makeLenses ''Transformable

data Physics = Physics {
  _velocity        :: Vec2f,
  _rotationalSpeed :: Float
}
makeLenses ''Physics

type TextureID = Int

data Graphics = Graphics {
  _textureID :: TextureID
}
makeLenses ''Graphics

type ComponentMap = Map.Map Entity

componentIntersection2 :: ComponentMap a -> ComponentMap b -> ComponentMap (a, b)
componentIntersection2 = Map.intersectionWith (\x y -> (x, y))

componentIntersection3 :: ComponentMap a -> ComponentMap b -> ComponentMap c -> ComponentMap (a, b, c)
componentIntersection3 a b = Map.intersectionWith (\(x, y) z -> (x, y, z)) (componentIntersection2 a b)

componentIntersection4 :: ComponentMap a -> ComponentMap b -> ComponentMap c -> ComponentMap d -> ComponentMap (a, b, c, d)
componentIntersection4 a b c = Map.intersectionWith (\(x, y, z) w -> (x, y, z, w)) (componentIntersection3 a b c)

data World = World {
  _window         :: Maybe RenderWindow,
  _oldKeysDown    :: [KeyCode],
  _curKeysDown    :: [KeyCode],

  _textures       :: [Texture],

  _lastID         :: Entity,

  _transformables :: ComponentMap Transformable,
  _physics        :: ComponentMap Physics,
  _graphics       :: ComponentMap Graphics
}
makeLenses ''World

type WorldState = State World

defWorld :: World
defWorld = World {
  _window = Nothing,
  _oldKeysDown = [],
  _curKeysDown = [],

  _textures = [],

  _lastID = 0,

  _transformables = Map.empty,
  _physics = Map.empty,
  _graphics = Map.empty
}

-- Adding entities / components
newEntity :: WorldState Entity
newEntity = lastID <+= 1

addComponent :: Entity -> c -> ASetter' World (ComponentMap c) -> WorldState ()
addComponent e c l = l %= Map.insert e c

removeComponent :: Entity -> ASetter' World (ComponentMap c) -> WorldState ()
removeComponent e l = l %= Map.delete e

-- Textures
getWorldTexture :: World -> TextureID -> Maybe Texture
getWorldTexture World{_textures=textures} texID
  | texID >= texLength = Nothing
  | texID < 0 = Nothing
  | otherwise = Just $ textures !! texID
  where texLength = length textures

-- Input
worldIsKeyDown :: World -> KeyCode -> Bool
worldIsKeyDown w@World{_curKeysDown=keys} key = isJust (List.find (==key) keys)

worldIsKeyPressed :: World -> KeyCode -> Bool
worldIsKeyPressed w@World{_curKeysDown=curKeys, _oldKeysDown=oldKeys} key = isJust (List.find (==key) curKeys) && isNothing (List.find (==key) oldKeys)

worldIsKeyReleased :: World -> KeyCode -> Bool
worldIsKeyReleased w@World{_curKeysDown=curKeys, _oldKeysDown=oldKeys} key = isNothing (List.find (==key) curKeys) && isJust (List.find (==key) oldKeys)

-- Update
updateWorld :: Float -> WorldState ()
updateWorld dt = do
  return ()

-- Render
drawTexture :: Maybe RenderWindow -> Maybe Texture -> Vec2f -> Float -> IO ()
drawTexture (Just wnd) (Just t) pos rot = do
  spr <- err createSprite
  setTexture spr t True
  texRect <- getTextureRect spr
  let width = fromIntegral (iwidth texRect)
      height = fromIntegral (iheight texRect)

  setOrigin spr (Vec2f (width / 2) (height / 2))
  setPosition spr pos
  setRotation spr (toDegrees rot)
  drawSprite wnd spr Nothing
drawTexture _ _ _ _ = return ()

renderEntity :: World -> (Graphics, Transformable) -> IO ()
renderEntity w@World{_window=wnd}
            (Graphics{_textureID=gid}, Transformable{_position=pos, _rotation=rot}) = drawTexture wnd tex pos rot
  where tex = getWorldTexture w gid

renderWorld :: World -> IO ()
renderWorld w@World{_graphics=graphics, _transformables=transforms} = do
  let graphicsAndTransforms = componentIntersection2 graphics transforms
  mapM_ (renderEntity w) graphicsAndTransforms
