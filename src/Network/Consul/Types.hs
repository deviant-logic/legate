{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Consul.Types where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Aeson
-- import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Text (Text)
import           Network.Consul.Util

data Service = Service {
      _svcId   :: Maybe Text,
      _svcName :: Text,
      _svcTags :: [Text],
      _svcPort :: Int,
      _svcCheck :: Maybe Check
} deriving (Eq, Ord, Show)

data Check = Script {
      _chkScript :: Text,
      _chkInterval :: Duration,
      _chkNotes :: Maybe Text
} | TTL {
      _chkTTL :: Duration,
      _chkNotes :: Maybe Text
} deriving (Eq, Ord, Show)

-- TODO: Do better than Text for Duration
type Duration = Text
    
instance FromJSON Service where
    parseJSON (caseFoldKeys -> Object o) =
        do _svcName  <- o .:  "name"
           _svcId    <- o .:? "id"
           _svcTags  <- o .:? "tags" .!= []
           _svcPort  <- o .:? "port" .!= 0
           _svcCheck <- o .:? "check"
           return Service {..}

instance ToJSON Service where
    toJSON Service {..} = object . execWriter $
        do tell ["name" .= _svcName,
                 "port" .= _svcPort,
                 "tags" .= _svcTags]
           tell $ maybe [] ((:[]) . ("id" .=))    _svcId
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
