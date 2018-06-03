
module Main where

import Protolude

import Options.Applicative
import Config (readConfig, parseConfig)

import Network.P2P (initNode)

main :: IO ()
main = do
  -- Read config from config file
  defaultConfig <- readConfig "config"
  -- Override config with cmd line arguments
  config <- execParser $ info (parseConfig defaultConfig) mempty
  -- Boot the nanocoin node
  initNode config
