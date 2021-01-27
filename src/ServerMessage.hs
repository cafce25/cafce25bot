{-# LANGUAGE RecordWildCards #-}
module ServerMessage (ServerMessage(..), Prefix(..), serverMessageP) where

import Control.Monad (replicateM, void)
import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Void (Void)
type Parser = Parsec Void String

data ServerMessage = Msg { msgPrefix :: Maybe Prefix
                         , msgCommand :: Command
                         , msgParameters :: [Parameter]
                         } deriving (Eq, Show)

data Prefix = Prefix { prefixUser :: Maybe User
                     , prefixHost :: Host
                     } deriving (Eq, Show)

type Command = String
type Parameter = String
type User = String
type Host = String

serverMessageP :: Parser ServerMessage
serverMessageP = do
    msgPrefix <- optional $ char ':' *> prefixP <* some (satisfy (== ' '))
    msgCommand <- commandP
    void $ some $ satisfy (== ' ')
    msgParameters <- paramsP
    pure Msg{..}

prefixP :: Parser Prefix
prefixP = Prefix <$> optional userP <*> some (noneOf (" " :: String))

userP :: Parser User
userP = try $ do
  u <- some alphaNumChar
  void $ string ("!" ++ u ++ "@" ++ u ++ ".")
  pure u

commandP :: Parser String
commandP = some letterChar <|> replicateM (3::Int) (digitChar :: Parser Char)

paramsP :: Parser [String]
paramsP = (:[]) <$> (char ':' *> many anySingle)
    <|> try (do
        tok <- many $ anySingleBut ' '
        void $ some $ satisfy (== ' ')
        rest <- paramsP
        return $ tok:rest)
    <|> (:[]) <$> many (anySingleBut ' ')


