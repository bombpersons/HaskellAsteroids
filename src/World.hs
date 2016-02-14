module World where
import qualified Data.List     as List
import qualified Data.Map      as Map
import           Data.Maybe
import           SFML.Graphics
import           SFML.Window

import           Utils

-- Entity system
data Entity = Entity {
  eTexture       :: Maybe Texture,
  ePosition      :: Vec2f,
  eRotation      :: Float,
  eVelocity      :: Vec2f,
  eRotationSpeed :: Float
}

emptyEntity :: Entity
emptyEntity = Entity {
                eTexture = Nothing,
                ePosition = Vec2f 0 0,
                eRotation = 0,
                eVelocity = Vec2f 0 0,
                eRotationSpeed = 0
}

data World = World { wWindow           :: RenderWindow,
                     wOldKeysDown      :: [KeyCode],
                     wCurKeysDown      :: [KeyCode],
                     wWorldRect        :: FloatRect,

                     wPlayer           :: Entity,
                     wPlayerShootTimer :: Float,

                     wAsteroids        :: [Entity],
                     wBullets          :: [Entity]
}

-- Input
worldIsKeyDown :: World -> KeyCode -> Bool
worldIsKeyDown w@World{wCurKeysDown=keys} key = isJust (List.find (==key) keys)

worldIsKeyPressed :: World -> KeyCode -> Bool
worldIsKeyPressed w@World{wCurKeysDown=curKeys, wOldKeysDown=oldKeys} key = isJust (List.find (==key) curKeys) && isNothing (List.find (==key) oldKeys)

worldIsKeyReleased :: World -> KeyCode -> Bool
worldIsKeyReleased w@World{wCurKeysDown=curKeys, wOldKeysDown=oldKeys} key = isNothing (List.find (==key) curKeys) && isJust (List.find (==key) oldKeys)

-- Update
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
  in e{ePosition = Vec2f newPx newPy, eRotation = r + rs * dt}

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

updateEntity :: World -> Float -> Entity -> Entity
updateEntity = moveAtVelocity

updatePlayer :: World -> Float -> Entity -> Entity
updatePlayer world dt = moveAtVelocity world dt .
                        moveBackAndForwardKeys 500 world dt .
                        turnLeftRightKeys (pi*2) world dt

playerShoot :: World -> Float -> Entity -> World
playerShoot w@World{wBullets=bullets, wPlayerShootTimer=timer} dt p@Entity{ePosition=pPos, eRotation=pRot}
  | worldIsKeyPressed w KeySpace && timer > 0 = w{wPlayerShootTimer=timer - dt}
  | otherwise = w{wBullets=newBullet:bullets, wPlayerShootTimer = 1}
  where newBullet = emptyEntity{ePosition = pPos, eRotation=pRot, eVelocity= flip multVec 200 . getForwardVecFromRotation $ pRot}

collideBullets :: World -> World
collideBullets world@World{wBullets=bullets} = foldr collideBullet world bullets
  where collideBullet e world = world

updateWorld :: World -> Float -> World
updateWorld w@World{wPlayer=player, wAsteroids=asteroids, wBullets=bullets} dt = worldAfterUpdate
  where
    updatedPlayer = updatePlayer w dt player
    updatedAsteroids = fmap (updateEntity w dt) asteroids
    updatedBullets = fmap (updateEntity w dt) bullets
    worldAfterUpdate = w{wPlayer=updatedPlayer, wAsteroids=updatedAsteroids, wBullets=updatedBullets}
    --worldAfterShoot = playerShoot worldAfterUpdate dt updatedPlayer
    --worldAfterBulletCollision = collideBullets worldAfterShoot

drawSpriteAtPos :: World -> Entity -> IO ()
drawSpriteAtPos world@World{wWindow=wnd, wWorldRect=FloatRect wx wy ww wh} e@Entity{eTexture=t} =
  case t of Nothing -> return ()
            Just tex -> do
              spr <- err createSprite
              setTexture spr tex True
              texRect <- getTextureRect spr
              let width = fromIntegral (iwidth texRect)
                  height = fromIntegral (iheight texRect)
              let pos = ePosition e + divVec (Vec2f wx wy) 2

              setOrigin spr (Vec2f (width / 2) (height / 2))
              setPosition spr pos
              setRotation spr (toDegrees (eRotation e))
              drawSprite wnd spr Nothing

renderWorld :: World -> IO ()
renderWorld world@World{wPlayer=player, wAsteroids=asteroids} = do
  mapM_ (drawSpriteAtPos world) asteroids
  drawSpriteAtPos world player
