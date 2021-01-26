{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

--import System.Environment (getArgs)
import Control.Exception (bracket)
import Control.Monad (replicateM, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.State --(StateT(..), runStateT) 
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import qualified Network.Socket as N
import System.Exit (exitSuccess, exitFailure)
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String
data ServerMessage = Msg { msgPrefix :: Maybe Prefix
                         , msgCommand :: Command
                         , msgParameters :: [Parameter]
                         } deriving Show

data Prefix = Prefix { prefixUser :: Maybe User
                     , prefixHost :: Host
                     } deriving Show

type Command = String
type Parameter = String
type User = String
type Host = String
data Bot = Bot { botConfig :: Config
               , botHandle :: Handle
               --, botRoles :: Map User Role
               } deriving Show

data Config = Config { confBotName :: String
                     , confBotAuth :: String
                     , confChannel :: String
                     , confServer  :: String
                     , confPort    :: N.PortNumber
                     } deriving Show
type Role = String
type Net = StateT Bot IO

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


-- Configuration options
myServer, myNick, myChannel :: String
myServer = "irc.chat.twitch.tv"
myNick   = "cafce25bot"
myChannel = "#cafce25"
myPort :: N.PortNumber
myPort   = 6667


-- Toplevel program
main :: IO ()
main = bracket startup teardown loop
  where teardown = hClose . botHandle
        loop st = evalStateT run st
          {-
    listen h
            -}

startup :: IO Bot
startup = do
    authToken <- readFile "auth.cafce25bot"
    let config = Config { confBotName = myNick
                        , confBotAuth = authToken
                        , confServer = myServer
                        , confPort = myPort
                        , confChannel = myChannel
                        }
    Bot config <$> connectTo (confServer config) (confPort config)
    

run :: Net ()
run = do
  gets (confBotAuth . botConfig) >>= write "PASS"
  gets (confBotName . botConfig) >>= write "NICK"
  gets (confChannel . botConfig) >>= write "JOIN"
  listen

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo hostname portnumber = do
    addr : _ <- N.getAddrInfo Nothing (Just hostname) (Just $ show portnumber)
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode


-- Send a message to a handle
write :: String -> String -> Net ()
write cmd args = do
    -- compose message from cmd and args
    h <- gets botHandle
    let msg = cmd ++ " " ++ args ++ "\r\n"
    -- Send message on the wire
    liftIO $ hPutStr h msg
    -- Show sent message on the command line
    liftIO $ if "PASS" `isPrefixOf` msg then putStrLn $ "> PASS oauth:" ++ replicate (length msg - 6) '*' else putStr ("> " ++ msg)

-- Process each line from the server
listen :: Net ()
listen = forever $ do
-- do forever read a line from the handle and put that line to stdout
    h <- gets botHandle
    msg' <- liftIO $ do line <- hGetLine h
                        putStrLn line
                        msg <- parseMessage (init line)
                        print msg
                        pure msg
    handleServerMessage msg'
    where forever :: Net () -> Net ()
          forever a = do a; forever a
          --cleanup the server messages (drop metadata)
          --parseServerMessage
          parseMessage :: String -> IO ServerMessage
          parseMessage x = case parse serverMessageP myServer x of
                             Right m -> pure m
                             Left e -> print e >> exitFailure
          --dispatch a single command
          --check for ping so we can respond with pong
          --send a pong responding with whatever the server sent us

handleServerMessage :: ServerMessage -> Net ()
handleServerMessage Msg{msgCommand="PING", msgParameters=[m]} = write "PONG" (':':m)
handleServerMessage msg@Msg{msgCommand="PRIVMSG", msgParameters=[_ch, m]}
-- | "!quit" `isPrefixOf` m = write "QUIT" ":Exiting" >> exitSuccess
  | "!echo " `isPrefixOf` m = write "PRIVMSG" (myChannel ++ " : " ++ drop 6 m)
  | otherwise = pure ()
  where user = pure msg >>= msgPrefix >>= prefixUser
handleServerMessage _ = pure ()
