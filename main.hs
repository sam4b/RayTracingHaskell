import Data.Maybe
import System.IO
import Data.Map (Map)
import Data.List
import Data.Function


data Vec3 = Vec3 Float Float Float 

instance Show Vec3 where
    show (Vec3 x y z) = show x ++ ", " ++ show y ++ ", " ++ show z
    
multiplyVector :: Vec3 -> Float -> Vec3
multiplyVector (Vec3 x y z) a = Vec3 (x * a) (y * a) (z * a)

unitVector :: Vec3 -> Vec3
unitVector v@(Vec3 x y z) = multiplyVector v (1.0 / sqrt (x*x + y*y + z*z))

addVector :: Vec3 -> Vec3 -> Vec3
addVector (Vec3 a b c) (Vec3 d e f) = Vec3 (a + d) (b + e) (c + f)

subtractVector :: Vec3 -> Vec3 -> Vec3
subtractVector (Vec3 a b c) (Vec3 d e f) = Vec3 (a - d) (b - e) (c - f)

data Sphere = Sphere Float Vec3
data Ray = Ray Vec3 Vec3

raySphereIntersection :: Sphere -> Ray -> Maybe Float
raySphereIntersection (Sphere radius center) (Ray origin direction) 
    | discriminant < 0 = Nothing
    | otherwise = Just t
  where
    oc = subtractVector center origin
    a = dotProduct direction direction
    b = -2.0 * dotProduct direction oc
    c = dotProduct oc oc - radius * radius
    discriminant = b * b - 4 * a * c
    t = (-b - sqrt discriminant) / (2 * a)

rayAt :: Ray -> Float -> Vec3
rayAt (Ray origin direction) t = addVector origin (multiplyVector direction t)
color :: Ray -> Maybe Float -> Vec3

color ray (Just t) = unitVector (rayAt ray t) `subtractVector` (Vec3 0 0 (-1))  -- Red if there is an intersection
color ray Nothing  = Vec3 0 0 0


intersects :: Ray -> [Sphere] -> [Maybe Float]
intersects ray [] = []
intersects ray (sphere:spheres) = [raySphereIntersection sphere ray] ++ intersects ray spheres

runIntersectionTests :: Ray -> [Sphere] -> Maybe Float
runIntersectionTests ray (sphere:spheres) 
    | null intersections = Nothing
    | otherwise = Just (head (sort intersections))
    where 
        ls = intersects ray (sphere:spheres)
        intersections = [x | Just x <- ls]


rayTrace :: [(Float, Float)] -> [Sphere] -> [Vec3]
rayTrace [] _ = []
rayTrace (coord:coords) spheres = [col] ++ rayTrace coords spheres
    where
        ray = Ray (Vec3 (fst coord) (snd coord) 0) (Vec3 0 0 (-1))
        col = color ray (runIntersectionTests ray spheres)

vec3ToString :: Vec3 -> String
vec3ToString (Vec3 x y z) = show (round (x * 255.999)) ++ " " ++ show (round (y*255.999)) ++ " " ++ show (round (z*255.999))
        
writeListToFile :: FilePath -> [Vec3] -> IO ()
writeListToFile filePath items = do
    handle <- openFile filePath WriteMode
    mapM_ (hPutStrLn handle) ["P3\n256 256\n255"] -- Write each item with a newline
    mapM_ (hPutStrLn handle . vec3ToString) items  -- Write each item with a newline
    hClose handle

main :: IO ()
main = do
    let pixelCoords = [(x, y) | x <- [1..256], y <- [1..256]]
    let spheres = [Sphere 5 (Vec3 0 0 (-1)) , Sphere 15 (Vec3 15 15 (-1)), Sphere 15 (Vec3 50 50 (-50))]
    let colors = rayTrace pixelCoords spheres
    writeListToFile "output.ppm" colors
    
