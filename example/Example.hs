{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude

import Control.Distributed.Process
import Control.Distributed.Process.Node

import Network.P2P (createLocalNode)

main :: IO ()
main = do
  (Right localNode) <- createLocalNode "127.0.1.1" "0"

  pid <- forkProcess localNode (listener)
  runProcess localNode (sayer pid)

listener :: Process ()
listener = forever $ do
  recvdStr <- expect :: Process [Char]
  putStrLn recvdStr

sayer :: ProcessId -> Process ()
sayer pid = forever $ do
  str :: [Char] <- toS <$> liftIO getLine
  send pid str
