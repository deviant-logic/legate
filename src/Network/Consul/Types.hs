{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Consul.Types where

import Control.Applicative
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Data.Text (Text)
import Network.Consul.Util

data Service = Service {
      _svcTags :: [Text],
      _svcPort :: Int,
      _svcAddress :: Maybe String,
      _svcCheck :: Maybe Check
} deriving (Eq, Ord, Show)

data Check = Script {
      _chkScript :: Text,
      _chkInterval :: Duration,
      _chkNotes :: Maybe Text
}          | TTL {
      _chkTTL :: Duration,
      _chkNotes :: Maybe Text
} deriving (Eq, Ord, Show)

-- TODO: Do better than Text for Duration
type Duration = Text

data Register a = Register {
      _regName  :: String,
      _regId    :: Maybe String,
      _regThing :: a
}               | DeRegister {
      _regName  :: String
} deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (Register a) where
    parseJSON (caseFoldKeys -> Object o) =
        do _regName <- o .: "name"
           _regId   <- o .:? "id"
           _regThing <- parseJSON (Object o)
           return Register {..}

instance ToJSON a => ToJSON (Register a) where
    toJSON Register {..} = Object $ o <> o'
        where (Object o)  = object $ ("name" .= _regName : idOrNot)
              (Object o') = toJSON _regThing
              idOrNot = maybeToList . fmap ("id".=) $ _regId

instance FromJSON Service where
    parseJSON (caseFoldKeys -> Object o) =
        do _svcTags    <- o .:? "tags" .!= []
           _svcPort    <- o .:? "port" .!= 0
           _svcAddress <- o .:? "address"
           _svcCheck   <- o .:? "check"
           return Service {..}

instance ToJSON Service where
    toJSON Service {..} = object . execWriter $
        do tell ["port" .= _svcPort,
                 "tags" .= _svcTags]
           tell $ maybe [] ((:[]) . ("address" .=)) _svcAddress
           tell $ maybe [] ((:[]) . ("check" .=)) _svcCheck

instance FromJSON Check where
    parseJSON (caseFoldKeys -> Object o) =
        do _chkNotes <- o .:? "notes"
           (do _chkTTL   <- o .: "ttl"
               return TTL {..}
            <|> 
            do _chkScript   <- o .: "script"
               _chkInterval <- o .: "interval"
               return Script {..})

instance ToJSON Check where
    toJSON chk = object . execWriter $
        do case chk of
             Script {..} -> tell ["script"   .= _chkScript,
                                 "interval" .= _chkInterval]
             TTL {..}    -> tell ["ttl"      .= _chkTTL]
           tell $ maybe [] ((:[]) . ("notes" .=)) $ _chkNotes chk

data KV = GetKey String | DelKey String | PutKey String ByteString

