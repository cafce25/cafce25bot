{-# LANGUAGE ApplicativeDo #-}

module Main where

import System.IO
import qualified Network.Socket as N

import Data.List
import Data.Maybe (fromJust)
import Data.Void (Void)
import System.Environment (getArgs)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- Configuration options
myServer = "irc.chat.twitch.tv" :: String
myPort   = 6667 :: N.PortNumber
myNick   = "cafce25"


-- Toplevel program
main :: IO ()
main = do
    authToken <- readFile "auth"
    -- connect to the server and returns a handle to the underlying socket
    h <- connectTo myServer myPort
    write h "PASS" authToken 
    write h "NICK" myNick
    write h "JOIN" ('#':myNick)
    listen h

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo hostname portnumber = do
    -- resolve our hostname to a valid adrrinfo struct
    addr : _ <- N.getAddrInfo Nothing (Just hostname) (Just $ show portnumber)
    -- create a socket to the address resolved earlier
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    -- connect to the socket
    N.connect sock (N.addrAddress addr)
    -- convert the socket to a Handle
    N.socketToHandle sock ReadWriteMode -- Socket -> IOMode -> IO HandleSource


-- Send a message to a handle
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    -- compose message from cmd and args
    let msg = cmd ++ " " ++ args ++ "\r\n"
    -- Send message on the wire
    hPutStr h msg
    -- Show sent message on the command line
    if "PASS" `isPrefixOf` msg then putStrLn "> PASS oauth:******************************" else putStr ("> " ++ msg)



-- Process each line from the server
listen :: Handle -> IO ()
listen h = forever $ do
-- do forever read a line from the handle and put that line to stdout
    line <- hGetLine h
    putStrLn line
    where forever :: IO () -> IO ()
          forever a = do a; forever a
