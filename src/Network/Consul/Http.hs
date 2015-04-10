{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Network.Consul.Http where

import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Network.Consul.Types
import           Network.HTTP.Client (defaultManagerSettings, managerRawConnection, host)
import           Network.HTTP.Client.Internal (Connection, makeConnection, noProxy, managerProxySecure, managerProxyInsecure, managerModifyRequest)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS
import           Network.Wreq
import qualified Network.Wreq.Session as NWS

makeUnixSocketConnection :: String -> Int -> IO Connection
makeUnixSocketConnection path chunkSize = bracketOnError
    (socket AF_UNIX Stream 0)
    (sClose)
    (\sock -> do
        connect sock (SockAddrUnix path)
        makeConnection
            (NBS.recv sock chunkSize)
            (NBS.sendAll sock)
            (sClose sock))

makeUrl :: ConsulPath -> String
makeUrl (HttpPath p)  = "http://" ++ p
makeUrl (HttpsPath p) = "https://" ++ p
makeUrl (UnixPath _)  = "http://localhost:0"

registerService :: NWS.Session -> ConsulPath -> Register Service -> IO ()
registerService s cp r@Register {..} = void $ NWS.put s ((makeUrl cp) ++ "/v1/agent/service/register") $ encode r
registerService s cp (DeRegister name) = void $ NWS.get s ((makeUrl cp) ++ "/v1/agent/service/deregister/" ++ name) 

registerCheck :: NWS.Session -> ConsulPath -> Register Check -> IO ()
registerCheck s cp r@Register {..} = void $ NWS.put s ((makeUrl cp) ++ "/v1/agent/check/register") $ encode r
registerCheck s cp (DeRegister name) = void $ NWS.get s ((makeUrl cp) ++ "/v1/agent/check/deregister/" ++ name) 

withService :: NWS.Session -> ConsulPath -> Register Service -> IO a -> IO a
withService sess cp s@Register {..} io = 
    bracket_ (registerService sess cp s)
             (registerService sess cp . DeRegister $ fromMaybe _regName _regId)
             io

kv :: NWS.Session -> ConsulPath -> KV -> IO ByteString
kv s cp thing = do resp <- case thing of
                           GetKey key -> NWS.getWith opts s (url' key)
                           PutKey key val -> NWS.putWith opts s (url' key) val
                           DelKey key -> NWS.deleteWith opts s (url' key) >>= return . fmap (const "")
                   return $ resp ^. responseBody <> "\n"
    where opts     = defaults & param "raw" .~ [""]
          url      = makeUrl cp
          url' key = url ++ "/v1/kv/" ++ key
