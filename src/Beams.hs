module Beams (
        Beams,
        new,
        addBeam,
        refresh,
        output
) where 

--MVar stuff
import Control.Concurrent
import Data.Time.Clock.POSIX
import System.IO
import Data.List
--RegEx stuff
import Data.Array
import Text.Regex 
import Text.Regex.Base

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

addBeam :: Beams -> String -> IO ()
addBeam (Beams m) coords = createBeam . getCoords $ coords
        where   createBeam (Just v) = do
                        ts <- getTS
                        beams <- takeMVar m
                        putMVar m (reduceBeams $ (Photon v Constants.radius ts) : beams)
                createBeam Nothing = do
                        return ()

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

regex = mkRegex "^([+-]?([0-9]*\\.)?[0-9]+) ([+-]?([0-9]*\\.)?[0-9]+)$"

isValid :: String -> Bool
isValid = matchTest regex

extractCoords :: String -> Vector
extractCoords str = Vector (li !! 1) (li !! 3)
        where li = map read . map fst . elems . head $ matchAllText regex str

getCoords :: String -> Maybe Vector
getCoords str
        | isValid str   = Just $ extractCoords str
        | otherwise     = Nothing
