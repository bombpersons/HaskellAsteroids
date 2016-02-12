module Main where
import qualified Data.Map      as Map
import           SFML.Graphics
import           SFML.Window

data EntityID = EntityID Int

data Velocity = Velocity Vec2f
data Position = Position Vec2f

data Entities = Entities { velocities :: Map.Map EntityID Velocity,
                           positions  :: Map.Map EntityID Position }

main :: IO ()
main = do
  -- Make a window.
  let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
  wnd <- createRenderWindow (VideoMode 640 480 32) "Haskell Asteroids" [SFDefaultStyle] ctxSettings

  -- Make entities.
  let ent = Entities { velocities = Map.empty, positions = Map.empty}

  -- Loop
  loop wnd ent
  destroy wnd

loop :: RenderWindow -> Entities -> IO ()
loop wnd ent = do


  evt <- waitEvent wnd
  case evt of
    Nothing -> return ()
    Just SFEvtClosed -> return ()
    _ -> loop wnd ent
