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
import qualified Network.Socket as N
import System.Exit ({-exitSuccess,-} exitFailure)
import System.IO
import ServerMessage
import Text.Megaparsec (parse)

data Bot = Bot { botConfig :: BotConfig
               , botHandle :: Handle
               --, botRoles :: Map User Role
               } deriving Show

type Role = String
type Net = StateT Bot IO


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
                        myServer <- gets $ confServer . botConfig
                        msg <- parseMessage (init line)
                        print msg
                        pure msg
    handleServerMessage msg'
    where --cleanup the server messages (drop metadata)
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
