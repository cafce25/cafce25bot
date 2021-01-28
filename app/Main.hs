{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

--import System.Environment (getArgs)
import BotConfig
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.State --(StateT(..), runStateT)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Network.Socket as N
import System.Exit (exitSuccess, exitFailure)
import System.IO
import ServerMessage
import Text.Megaparsec (parse)

data Bot = Bot { botConfig :: BotConfig
               , botHandle :: Handle
               --, botRoles :: Map User Role
               } deriving Show

type Role = String
type Net = StateT Bot IO
type User = String


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
    config <- getConfig
    Bot config <$> connectTo (confServer config) (fromIntegral $ confPort config)


run :: Net ()
run = do
  gets (confAuth . botConfig) >>= write "PASS"
  gets (confBotUser . botConfig) >>= write "NICK"
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
    myServer <- gets $ confServer . botConfig
    msg' <- liftIO $ do line <- hGetLine h
                        putStrLn line
                        msg <- parseMessage (init line) myServer
                        print msg
                        pure msg
    handleServerMessage msg'
    where --cleanup the server messages (drop metadata)
          --parseServerMessage
          parseMessage x s = case parse serverMessageP s x of
                             Right m -> pure m
                             Left e -> print e >> exitFailure
          --dispatch a single command
          --check for ping so we can respond with pong
          --send a pong responding with whatever the server sent us

isInGroup :: Role -> User -> Map User Role -> Bool
isInGroup r = ((r ==).) . M.findWithDefault "user"

isMod :: User -> Map User Role -> Bool
isMod = isInGroup "moderator"

isStreamer :: User -> Map User Role -> Bool
isStreamer = isInGroup "streamer"

getRole :: Map User Role -> Maybe User -> Maybe Role
getRole m = (>>= (`M.lookup` m))

handleServerMessage :: ServerMessage -> Net ()
handleServerMessage Msg{msgCommand="PING", msgParameters=[m]} = write "PONG" (':':m)
handleServerMessage msg@Msg{msgCommand="PRIVMSG", msgParameters=[_ch, m]}
  | "!quit" `isPrefixOf` m = do
    roles <- gets $ confRoles . botConfig
    if getRole roles user `elem` map Just [{-"moderator",-} "streamer"]
       then write "QUIT" ":Exiting" >> liftIO exitSuccess
       else write "PRIVMSG" "#cafce25 :need to be moderator to use !quit"
  | "!echo " `isPrefixOf` m = gets (confChannel . botConfig) >>= \myChannel -> write "PRIVMSG" (myChannel ++ " : " ++ drop 6 m)
  | otherwise = pure ()
  where user = pure msg >>= msgPrefix >>= prefixUser
handleServerMessage _ = pure ()
