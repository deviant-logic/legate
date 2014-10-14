{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Network.Consul.Http where

import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe
import qualified Data.Text as T
import           Network.Consul.Types
import           Network.Wreq


registerService :: String -> Register Service -> IO ()
registerService url r@Register {..} = void $ put (url ++ "/v1/agent/service/register") $ encode r
registerService url (DeRegister name) = void $ get (url ++ "/v1/agent/service/deregister/" ++ name) 

registerCheck :: String -> Register Check -> IO ()
registerCheck url r@Register {..} = void $ put (url ++ "/v1/agent/check/register") $ encode r
registerCheck url (DeRegister name) = void $ get (url ++ "/v1/agent/check/deregister/" ++ name) 

withService :: String -> Register Service -> IO a -> IO a
withService url s@Register {..} io =
    bracket_ (registerService url s)
             (registerService url . DeRegister $ fromMaybe _regName _regId)
             io
