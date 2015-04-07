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
import           Network.Wreq
import           System.IO
import           System.Posix.Signals


registerService :: String -> Register Service -> IO ()
registerService url r@Register {..} = void $ put (url ++ "/v1/agent/service/register") $ encode r
registerService url (DeRegister name) = void $ get (url ++ "/v1/agent/service/deregister/" ++ name) 

registerCheck :: String -> Register Check -> IO ()
registerCheck url r@Register {..} = void $ put (url ++ "/v1/agent/check/register") $ encode r
registerCheck url (DeRegister name) = void $ get (url ++ "/v1/agent/check/deregister/" ++ name) 

withService :: String -> Register Service -> IO a -> IO a
withService url s@Register {..} io =
    bracket_ (registerService url s)
             (deregisterService)
             (installHandler sigTERM (Catch deregisterService) Nothing >> io)
  where
    deregisterService = do
        putStrLn "Deregistering service..."
        registerService url . DeRegister $ fromMaybe _regName _regId

kv :: String -> KV -> IO ByteString
kv url thing = do resp <- case thing of
                           GetKey key -> getWith opts (url' key)
                           PutKey key val -> put (url' key) val
                           DelKey key -> delete (url' key) >>= return . fmap (const "")
                  return $ resp ^. responseBody <> "\n"
    where opts = defaults & param "raw" .~ [""]
          url' key = url ++ "/v1/kv/" ++ key
