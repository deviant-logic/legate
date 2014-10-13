
module Network.Consul.Util where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

caseFoldKeys :: Value -> Value
caseFoldKeys (Object o) = object [ T.toCaseFold k .= caseFoldKeys v
                                   | (k,v) <- HM.toList o]
caseFoldKeys (Array a)  = Array $ fmap caseFoldKeys a
caseFoldKeys v          = v
