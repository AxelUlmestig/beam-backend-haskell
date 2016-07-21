import Control.Monad.State  
import Data.Time.Clock.POSIX
import Control.Concurrent
import System.IO

import qualified Constants
import Vector
import Beam
import ReduceBeams
import RefreshBeams
import qualified Beams

main :: IO ()
main = do
        beams <- Beams.new
        mainLoop beams

mainLoop :: Beams.Beams -> IO ()
mainLoop beams = do
        input <- liftIO $ prompt "Please provide coordinates for a new Beam: "
        addBeam input beams
        setRefreshTimer beams
        mainLoop beams

addBeam input beams = do
        forkIO $ do 
                Beams.addBeam (toVector input) beams
                Beams.output print beams

setRefreshTimer beams = doLater (Constants.ageLimit * 10 ^ 6) $ do 
        Beams.refresh beams
        Beams.output print beams

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

doLater :: Int -> IO () -> IO ThreadId
doLater t io = forkIO $ threadDelay t >> io

toVector :: String -> Vector
toVector = listToVector . toDoubles
        where   toDoubles = map read . words
                listToVector (lat:lon:[]) = Vector lat lon
