{-# Language DataKinds, OverloadedStrings #-}
module Constants where

import Network.MQTT

radius = 5.0 :: Double
--ageLimit = 5 * 60 :: Int
ageLimit = 10 :: Int

listenTopic = "topic1" :: Topic
publishTopic = "topic2" :: Topic
mqttHost = "test.mosquitto.org" 
