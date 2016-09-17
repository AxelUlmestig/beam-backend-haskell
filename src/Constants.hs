{-# Language DataKinds, OverloadedStrings #-}
module Constants where

import Network.MQTT

radius = 5.0 :: Double
--ageLimit = 5 * 60 :: Int
ageLimit = 10 :: Int

listenTopic = "beam:publish" :: Topic
publishTopic = "beam:listen" :: Topic
mqttHost = "broker.mqttdashboard.com" 
