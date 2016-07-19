import Control.Monad.State  
import Data.Time.Clock.POSIX
import Control.Concurrent
import System.IO

import Vector
import Beam
import ReduceBeams
import RefreshBeams
import qualified Constants

type Beams = [Beam]

main :: IO ()
main = runStateT beamIO [] >> return ()
 
beamIO :: StateT Beams IO ()
beamIO = do
        input <- io $ prompt "Please provide coordinates for a new Beam: "
        ts <- io getTS
        addBeam (toVector input) ts
        beams <- get
        io . print . present $ beams
        --io . doLater Constants.ageLimit . print $ refreshBeams ts beams
        io . doLater (10 ^ 6) . print $ refreshBeams ts beams
        beamIO

addBeam :: Vector -> Int -> StateT Beams IO ()
addBeam v ts = do
        let photon = Photon v Constants.radius ts
        beams <- get
        put (reduceBeams $ photon : beams)

present :: Beams -> String
present = show . map showBeam
        where   showBeams = show . map showBeam
                showBeam beam = (getX . position $ beam, getY . position $ beam, radius beam)
                getX (Vector x y) = x
                getY (Vector x y) = y

getTS :: IO Int
getTS = fmap round getPOSIXTime

io :: IO a -> StateT Beams IO a
io = liftIO

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

toVector :: String -> Vector
toVector = listToVector . toDoubles
        where   toDoubles = map read . words
                listToVector (lat:lon:[]) = Vector lat lon

doLater :: Int -> IO () -> IO ThreadId
doLater t io = forkIO $ threadDelay t >> io
