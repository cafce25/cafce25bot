{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module BotConfig (BotConfig(..), getConfig) where

import Data.Aeson.TH
import Data.List
import Data.Map (Map)
import Data.Yaml
import Data.Yaml.Config
import Text.Casing

data BotConfig = BotConfig { confBotUser :: String
                           , confAuth    :: String
                           , confChannel :: String
                           , confServer  :: String
                           , confPort    :: Int
                           , confRoles   :: Map String String
                           } deriving Show

$(deriveJSON defaultOptions{ fieldLabelModifier = quietSnake . drop 4} ''BotConfig)

defaultConf :: Value
defaultConf
  = (\case {Right r -> r; Left _ -> error "default config is misformed"}) $ decodeEither'
    "server: irc.chat.twitch.tv\n\
    \port: 6667\n"

getConfig :: IO BotConfig
getConfig = do
  conf <- loadYamlSettings ["chatbot.yaml"] [defaultConf] useEnv
  let authS = confAuth conf
  if "oauth:" `isPrefixOf` authS
     then pure conf
     else do
       auth <- readFile authS
       pure conf{confAuth = auth}
