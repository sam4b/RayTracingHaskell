import Data.Maybe
import System.IO
import Data.Map (Map)
import Data.List
import Data.Function

import Vec3
import Distribution.Simple.Test (test)

data Sphere = Sphere Double Vec3
data Ray = Ray Vec3 Vec3

testIntersection :: Sphere -> Ray -> Maybe Float
testIntersection (Sphere radius center) (Ray origin direction) 
    | discriminant < 0 = Nothing 
    | otherwise = Just 5 --TBA
  where
    oc = subtractVector center origin
    a = dotProduct direction direction
    b = -2.0 * (dotProduct direction oc)
    c = (dotProduct oc oc) - (radius * radius)
    discriminant = b * b - 4 * a * c


color :: Maybe Float -> Ray -> Vec3
color (Just t) _ = Vec3 1 0 0
color Nothing (Ray origin direction) = (multiplyVector (Vec3 1.0 1.0 1.0) (1.0-a)) `addVector` (multiplyVector (Vec3 0.5 0.7 1.0) a)
    where
        a = 0.5 * (y + 1)
        unitDir = unitVector direction 
        (Vec3 _ y _ ) = unitDir
    
rayAt :: Ray -> Double -> Vec3
rayAt (Ray origin direction) t = addVector origin (multiplyVector direction t)

calculatePixelCenter :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Vec3
calculatePixelCenter pixelLoc pixeldu pixeldv x y = pixelLoc `addVector` (multiplyVector pixeldu x) `addVector` (multiplyVector pixeldv y)

getRayDirection :: Vec3 -> Vec3 -> Vec3
getRayDirection pixelCenter cameraCenter = pixelCenter `subtractVector` cameraCenter

rayTrace :: [(Double, Double)] -> Sphere -> (Double -> Double -> Vec3) -> Vec3 -> [Vec3] 
rayTrace [] _ _ _ = []
rayTrace (coord:coords) sphere pixelCenterFuncton cameraCenter = col : rayTrace coords sphere pixelCenterFuncton cameraCenter
    where
        ray = (Ray cameraCenter direction)
        direction = getRayDirection pixelCenter cameraCenter
        pixelCenter = pixelCenterFuncton (snd coord) (fst coord) 
        col = color (testIntersection sphere ray) ray

writeListToFile :: FilePath -> [Vec3] -> Double -> Double -> IO ()
writeListToFile filePath items width height = do
    handle <- openFile filePath WriteMode
    mapM_ (hPutStrLn handle) (["P3\n"] ++ [show width] ++ [" "] ++ [show height] ++ ["\n255"])
    mapM_ (hPutStrLn handle . vec3ToString) items
    hClose handle



main :: IO ()
main = do

    let imageWidth :: Double = 400.0
    let imageHeight :: Double = 225.0

--(0,0), (0,1), ... (0, 225), (1, 0) , ... (1, 225)
    let pixelCoords = [(y,x) | y <- [0..imageHeight-1], x <- [0..imageWidth-1]]
    let sphere = Sphere 0.5 (Vec3 0 0 (-1))

    let viewportHeight ::Double = 2.0
    let viewportWidth = viewportHeight * (imageWidth / imageHeight)

    let cameraCenter = (Vec3 0 0 0)

    let viewportU = (Vec3 viewportWidth 0 0)
    let viewportV = (Vec3 0 (-1 * viewportHeight) 0)

    let pixeldu = multiplyVector viewportU (1.0 / imageWidth) 

    let pixeldv = multiplyVector viewportV (1.0 / imageHeight) 

    let focalLength = 1.0

    let viewportUpperLeft = cameraCenter `subtractVector` (Vec3 0 0 focalLength) `subtractVector` (multiplyVector viewportU 0.5) `subtractVector` (multiplyVector viewportV 0.5)

    let pixelLoc = addVector viewportUpperLeft (multiplyVector (pixeldu `addVector` pixeldv) 0.5)

    let pixelF = calculatePixelCenter pixelLoc pixeldu pixeldv --Partially apply so I don't have to pass all that around.

    let result = rayTrace pixelCoords sphere pixelF cameraCenter

    writeListToFile "output.ppm" result imageWidth imageHeight
    
