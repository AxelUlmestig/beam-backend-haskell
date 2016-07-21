module Beams (
        Beams,
        new,
        addBeam,
        refresh,
        output
) where 

import Control.Concurrent
import Data.Time.Clock.POSIX
import System.IO
import Data.List

import Vector
import Beam
import ReduceBeams
import RefreshBeams
import qualified Constants

newtype Beams = Beams (MVar [Beam])

new :: IO Beams
new = do
        m <- newMVar []
        return (Beams m)

addBeam :: Beams -> Vector -> IO ()
addBeam (Beams m) coords = do
        ts <- getTS
        beams <- takeMVar m
        putMVar m (reduceBeams $ (Photon coords Constants.radius ts) : beams)

refresh :: Beams -> IO ()
refresh (Beams m) = do
        ts <- getTS
        beams <- takeMVar m
        putMVar m (refreshBeams (ts - Constants.ageLimit) beams)

output :: Beams -> (String -> IO ()) -> IO ()
output (Beams m) io = do
        beams <- takeMVar m
        putMVar m beams
        io . present $ beams
        hFlush stdout

present :: [Beam] -> String
present = addBrackets . intercalate ", " . map beamToJSON
        where   addBrackets str = "[" ++ str ++ "]"

beamToJSON :: Beam -> String
beamToJSON beam = "{\"lat\": " ++ x ++ ", \"lon\": " ++ y ++ ", \"radius\": " ++ rad ++ "}"
        where   x = show . getX . position $ beam
                y = show . getY . position $ beam
                rad = show . radius $ beam
                getX (Vector x y) = x
                getY (Vector x y) = y

getTS :: IO Int
getTS = fmap round getPOSIXTime
