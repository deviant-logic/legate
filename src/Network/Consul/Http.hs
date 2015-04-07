{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Network.Consul.Http where

import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Network.Consul.Types
import           Network.HTTP.Client (defaultManagerSettings, managerRawConnection)
import           Network.HTTP.Client.Internal (Connection, makeConnection)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS
import           Network.Wreq

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
makeUrl (UnixPath p)  = "http://localhost:0"

makeOpts :: ConsulPath -> Options
makeOpts (UnixPath p) = defaults & manager .~ Left (defaultManagerSettings {  managerRawConnection = return $ \_ _ _ -> makeUnixSocketConnection p 4096 })
makeOpts _            = defaults

registerService :: ConsulPath -> Register Service -> IO ()
registerService cp r@Register {..} = void $ putWith (makeOpts cp) ((makeUrl cp) ++ "/v1/agent/service/register") $ encode r
registerService cp (DeRegister name) = void $ getWith (makeOpts cp) ((makeUrl cp) ++ "/v1/agent/service/deregister/" ++ name) 

registerCheck :: ConsulPath -> Register Check -> IO ()
registerCheck cp r@Register {..} = void $ putWith (makeOpts cp) ((makeUrl cp) ++ "/v1/agent/check/register") $ encode r
registerCheck cp (DeRegister name) = void $ getWith (makeOpts cp) ((makeUrl cp) ++ "/v1/agent/check/deregister/" ++ name) 

withService :: ConsulPath -> Register Service -> IO a -> IO a
withService cp s@Register {..} io =
    bracket_ (registerService cp s)
             (registerService cp . DeRegister $ fromMaybe _regName _regId)
             io

kv :: ConsulPath -> KV -> IO ByteString
kv cp thing = do resp <- case thing of
                           GetKey key -> getWith opts (url' key)
                           PutKey key val -> putWith opts (url' key) val
                           DelKey key -> deleteWith opts (url' key) >>= return . fmap (const "")
                 return $ resp ^. responseBody <> "\n"
    where opts     = makeOpts cp & param "raw" .~ [""]
          url      = makeUrl cp
          url' key = url ++ "/v1/kv/" ++ key
