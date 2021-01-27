{-# LANGUAGE TemplateHaskell #-}
module BotConfig (BotConfig(..), getConfig) where

--import Data.Yaml
--import Data.Yaml.Config
import Data.Aeson.TH
import Data.Char (toLower)
import qualified Network.Socket as N

data BotConfig = BotConfig { confBotName :: String
                           , confBotAuth :: String
                           , confChannel :: String
                           , confServer  :: String
                           , confPort    :: Int
                           } deriving Show

$(deriveJSON defaultOptions{ fieldLabelModifier = drop 4, constructorTagModifier = map toLower} ''BotConfig)

-- Configuration options
myServer, myNick, myChannel :: String
myServer = "irc.chat.twitch.tv"
myNick   = "cafce25bot"
myChannel = "#cafce25"
myPort :: N.PortNumber
myPort   = 6667

getConfig :: IO BotConfig
getConfig = do
  authToken <- readFile "auth.cafce25bot"
  pure BotConfig { confBotName = myNick
                 , confBotAuth = authToken
                 , confServer = myServer
                 , confPort = fromIntegral myPort
                 , confChannel = myChannel
                 }

