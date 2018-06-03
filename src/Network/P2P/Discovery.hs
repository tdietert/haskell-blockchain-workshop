{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.P2P.Discovery where

import Protolude

import Control.Distributed.Process
  ( Process
  , NodeId
  , WhereIsReply(..)

  , receiveWait
  , match
  , processNodeId
  , say
  , nsendRemote
  , register
  , getSelfNode
  , getSelfPid
  , whereisRemoteAsync
  )

import qualified Data.Set as S

import Network.P2P.Node

{-

Important datatypes:
  - NodeId
  - ProcessId
  - WhereIsReply

Important functions:
  - literally all of Control.Distributed.Process imports...

-}

-- | The name of a node's peer discovery process
peerDiscovery :: [Char]
peerDiscovery = "peer-discovery"

-- | Process that tries to discover all nodes with the 'NodeId' supplied.
peerDiscoveryProc
  :: NodeEnv    -- ^ Node Environment
  -> [NodeId]   -- ^ Nodes to discover
  -> Process ()
peerDiscoveryProc nodeEnv bootnodes = do
    -- Register local peer discovery process"
    register peerDiscovery =<< getSelfPid
    -- Discover all bootnodes
    mapM discoverPeer bootnodes
    -- Add self as a peer to the nodes known peers
    selfNid <- getSelfNode
    liftIO (addPeer nodeEnv (Peer selfNid))
    -- Forever wait for messages
    forever $ receiveWait
      [ match handleWhereIsReply
      , match handlePeers
      ]
  where
    -- Received from nodes we've send whereis messages to
    handleWhereIsReply :: WhereIsReply -> Process ()
    handleWhereIsReply (WhereIsReply _ mpid) = do
      case mpid of
        Nothing  -> say "Received 'Nothing' as pid"
        Just pid -> do
          -- Add peer to our peer list
          let peerNid = processNodeId pid
          liftIO (addPeer nodeEnv (Peer peerNid))
          -- Respond with all our known peers
          knownPeers <- liftIO (askPeers nodeEnv)
          nsendRemote peerNid peerDiscovery knownPeers

    handlePeers :: Peers -> Process ()
    handlePeers potentialNewPeers = do
      knownPeers <- liftIO (askPeers nodeEnv)
      let newPeers = potentialNewPeers `S.difference` knownPeers
      mapM_ (discoverPeer . unPeer) newPeers

-- | Process that discovers a single peer
discoverPeer
  :: NodeId
  -> Process ()
discoverPeer nid = do
  say $ "discoverPeer: " <> show nid
  whereisRemoteAsync nid peerDiscovery
