
module Config
  ( Config(..)
  , readConfig
  , parseConfig
  ) where

import Protolude hiding (option)

import Control.Monad (fail)

import Control.Distributed.Process (NodeId(..))

import Options.Applicative

import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Text as T

import Network.Socket (HostName, PortNumber)
import Network.Transport (EndPointAddress(..))

data Config = Config
  { hostname  :: HostName       -- ^ Hostname of node
  , port      :: Int            -- ^ Port to run p2p process on
  , bootnodes :: [NodeId]       -- ^ List of bootnode hostnames and ports
  , nodeKey   :: Maybe FilePath -- ^ Filepath to read node keys from
  }

-- Read config from config file
readConfig :: FilePath -> IO Config
readConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  Config <$> C.require cfg "node.hostname"
         <*> C.require cfg "node.port"
         <*> (parseBootnodes =<< C.require cfg "node.bootnodes")
         <*> C.lookup  cfg "node.key"

  where
    parseBootnodes nids' =
      forM nids' $ \nid' ->
        case mkNodeId' nid' of
          Nothing  -> fail "Cannot parse NodeId in bootnodes list"
          Just nid -> pure nid

-- Parse Config options via command line
parseConfig :: Config -> Parser Config
parseConfig config = Config
    <$> hostnameParser `fallback` (hostname config)
    <*> portParser     `fallback` (port config)
    <*> pure (bootnodes config)
    <*> nodeKeyParser
  where
    hostnameParser :: Parser (Maybe HostName)
    hostnameParser = optional $
      option auto $ long "hostname"
                 <> metavar "HOSTNAME"

    portParser :: Parser (Maybe Int)
    portParser = optional $
      option auto $ long "port"
                 <> short 'n'
                 <> metavar "PORT"

    nodeKeyParser :: Parser (Maybe FilePath)
    nodeKeyParser = optional $
      strOption $ long "key"
               <> short 'k'
               <> metavar "KEY"

fallback :: Parser (Maybe a) -> a -> Parser a
fallback parser x = flip fallback' x <$> parser
  where
    fallback' (Just y) _ = y
    fallback' Nothing y  = y

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkNodeId :: HostName -> PortNumber -> NodeId
mkNodeId hostname' portNum = do
  NodeId $ EndPointAddress $
    BS.intercalate ":" $ [toS hostname', show portNum, "0"]

mkNodeId' :: Text -> Maybe NodeId
mkNodeId' t =
  case T.splitOn ":" t of
    [hn,portStr] ->
      case readMaybe (toS portStr) of
        Nothing -> Nothing
        Just p  -> Just (mkNodeId (toS hn) p)
    _  -> Nothing
