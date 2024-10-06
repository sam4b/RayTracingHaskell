module Vec3 where

data Vec3 = Vec3 Double Double Double 

instance Show Vec3 where
    show (Vec3 x y z) = show x ++ ", " ++ show y ++ ", " ++ show z

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

multiplyVector :: Vec3 -> Double -> Vec3
multiplyVector (Vec3 x y z) a = Vec3 (x * a) (y * a) (z * a)

unitVector :: Vec3 -> Vec3
unitVector v@(Vec3 x y z) = multiplyVector v (1.0 / sqrt (x*x + y*y + z*z))

addVector :: Vec3 -> Vec3 -> Vec3
addVector (Vec3 a b c) (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)

subtractVector :: Vec3 -> Vec3 -> Vec3
subtractVector (Vec3 a b c) (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)

vec3ToString :: Vec3 -> String
vec3ToString (Vec3 x y z) = show (round (x * 255.999)) ++ " " ++ show (round (y*255.999)) ++ " " ++ show (round (z*255.999))
        