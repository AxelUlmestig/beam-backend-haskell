import Control.Monad.State  
import Data.Time.Clock.POSIX
import Control.Concurrent

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
        io $ putStr "Please provide coordinates for a new Beam: "
        coords <- io getCoords
        ts <- io getTS
        addBeam coords ts
        beams <- get
        io . print . present $ beams
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

getCoords :: IO Vector
getCoords = do
        let toDouble x = read x :: Double
        let toDoubles = map toDouble . words
        let listToVector (lat:lon:[]) = Vector lat lon
        let toVector = listToVector . toDoubles
        str <- getLine
        return $ toVector str

doLater :: Int -> IO () -> IO ThreadId
doLater ms io = forkIO $ threadDelay (ms * 1000) >> io
