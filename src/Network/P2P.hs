
module Network.P2P
  ( createLocalNode
  , initNode
  ) where

import Protolude

import Control.Distributed.Process.Node
  ( newLocalNode
  , LocalNode
  , initRemoteTable
  , runProcess
  , forkProcess
  )

import Data.Binary (decode)
import qualified Data.ByteString.Lazy as BSL

import Network.Socket (ServiceName, HostName)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Network.P2P.Discovery (peerDiscoveryProc)
import Network.P2P.Message (messagingProc)
import Network.P2P.Node (initNodeEnv)

import Address (Address, publicKeyToAddress)
import Block (genesisBlockchain)
import Config (Config(..))
import Cryptography (PrivateKey, privKeyToPubKey, newKeyPair)

import System.IO.Error (userError)

createLocalNode
  :: HostName
  -> ServiceName
  -> IO (Either Text LocalNode)
createLocalNode host port = do
  -- Create a TCP Transport (TCP conn between each remote process)
  eTransport <- createTransport host port (host,) defaultTCPParameters
  case eTransport of
    Left err -> pure $ Left $ show err
    Right transport -> Right <$>
      newLocalNode transport initRemoteTable

-- | Initialize the node, booting the p2p and messaging processes
initNode :: Config -> IO ()
initNode config = do
  -- Load node private key or generate one on the fly.
  nodePrivKey <- case nodeKey config of
    Nothing -> snd <$> newKeyPair
    Just fp -> decode <$> BSL.readFile fp

  -- Display address of node
  let nodeAddress = publicKeyToAddress (privKeyToPubKey nodePrivKey)
  putText $ "Node Address: " <> show nodeAddress

  -- Create NodeEnv and LocalNode
  eNodeEnv   <- first show <$> initNodeEnv nodePrivKey genesisBlockchain
  eLocalNode <- createLocalNode (hostname config) (show (port config))
  case (,) <$> eNodeEnv <*> eLocalNode of
    Left err -> throwIO (userError (show err))
    Right (nodeEnv, localNode) -> do
      -- Boot peer discovery and messaging processes
      forkProcess localNode $
        peerDiscoveryProc nodeEnv (bootnodes config)
      runProcess localNode $
        messagingProc nodeEnv
