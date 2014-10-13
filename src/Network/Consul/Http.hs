{-# LANGUAGE OverloadedStrings #-}


module Network.Consul.Http where

import           Control.Exception
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import           Network.Consul.Types
import           Network.Wreq


data Register a = Register a | DeRegister String
                  deriving (Eq, Ord, Show)

registerService :: String -> Register Service -> IO ()
registerService url (Register svc) = void $ put (url ++ "/v1/agent/service/register") $ encode svc
registerService url (DeRegister name) = void $ get (url ++ "/v1/agent/service/deregister/" ++ name) 

withService :: String -> Service -> IO a -> IO a
withService url s io = bracket_ (registerService url $ Register s)
                                (registerService url . DeRegister . T.unpack $
                                                 (case _svcId s of
                                                   Nothing -> _svcName s
                                                   Just sid -> sid))
                                io
