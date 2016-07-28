{-# Language DataKinds, OverloadedStrings #-}

module Main where
import Control.Concurrent

import MQTTWrapper
import qualified Constants
import qualified Beams
import Vector

listenTopic, publishTopic :: Topic
listenTopic = "topic1"
publishTopic = "topic2"
--listenTopic = Constants.listenTopic
--publishTopic = Constants.publishTopic

host :: String
host = Constants.mqttHost

main :: IO ()
main = do
        conf <- getConfig host
        forkIO $ runMQTT conf
        beams <- Beams.new
        mainLoop conf beams

mainLoop :: Config -> Beams.Beams -> IO ()
mainLoop config beams = do
        input <- listen config listenTopic
        forkIO . handleMsg config beams $ input
        mainLoop config beams

handleMsg :: Config -> Beams.Beams -> String -> IO ()
handleMsg config beams input = do
        addBeam config beams input
        setRefreshTimer config beams
        return ()

addBeam config beams input = do
        forkIO $ do 
                Beams.addBeam beams (toVector input) 
                Beams.output beams $ publish config publishTopic

toVector :: String -> Vector
toVector = listToVector . toDoubles
        where   toDoubles = map read . words
                listToVector (lat:lon:[]) = Vector lat lon
        
setRefreshTimer config beams = doLater (Constants.ageLimit * 10 ^ 6) $ do 
        Beams.refresh beams
        Beams.output beams $ publish config publishTopic

doLater :: Int -> IO () -> IO ThreadId
doLater t io = forkIO $ threadDelay t >> io
