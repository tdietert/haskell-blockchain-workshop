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
peerDiscoveryProc = undefined
  -- Discover all bootnodes
  -- Add self as a peer to the nodes known peers
  -- Register local peer discovery process"
  where
    handleWhereIsReply :: WhereIsReply -> Process ()
    handleWhereIsReply  = undefined

    handlePeers :: Peers -> Process ()
    handlePeers = undefined

-- | Process that discovers a single peer
discoverPeer
  :: NodeId
  -> Process ()
discoverPeer nid = do
  say $ "discoverPeer: " <> show nid
  whereisRemoteAsync nid peerDiscovery
