{-# Language DataKinds, OverloadedStrings #-}

module MQTTWrapper (
        getConfig,
        runMQTT,
        listen,
        publish,
        MQTT.Topic,
        MQTT.Config
) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString.Char8 (pack, unpack)

import qualified Network.MQTT as MQTT

getConfig :: String -> IO MQTT.Config
getConfig host = do
        cmds <- MQTT.mkCommands
        pubChan <- newTChanIO
        return $ (MQTT.defaultConfig cmds pubChan) { MQTT.cHost = host }

listen :: MQTT.Config -> MQTT.Topic -> IO String
listen conf topic = do
        qosGranted <- MQTT.subscribe conf [(topic, MQTT.Handshake)]
        msg <- atomically (readTChan $ MQTT.cPublished conf)
        return $ unpack . MQTT.payload . MQTT.body $ msg

publish :: MQTT.Config -> MQTT.Topic -> String -> IO ()
publish conf topic msg = do
        MQTT.publish conf MQTT.Handshake False topic (pack msg)

runMQTT :: MQTT.Config -> IO ()
runMQTT config = do
        terminated <- MQTT.run config
        print terminated
