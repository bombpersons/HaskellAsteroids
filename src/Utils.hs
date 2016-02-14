module Utils where
import           SFML.Window

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
