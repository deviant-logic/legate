{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens hiding (argument)
import           Data.Aeson
import qualified Data.Text as T
import           Network.Consul.Http
import           Network.Consul.Types
import           Network.Wreq
import           Options.Applicative

data GlobalOpts = GlobalOpts {
  _consulHost :: String,
  _consulPort :: Int,
  _consulSSL  :: Bool
}

defaultOpts = GlobalOpts "localhost" 8500 False

consulPath :: GlobalOpts -> String
consulPath (GlobalOpts host port ssl) = scheme ++ host ++ ":" ++ show port
  where scheme | ssl       = "https://"
               | otherwise = "http://"

svcParser = Service <$> optional (option text svcid)
                    <*> option text name
                    <*> many (option text tag)
                    <*> option auto port
                    <*> optional chkParser
  where svcid = long "id"   <> short 'i'
        name  = long "name" <> short 'n'
        tag   = long "tag"  <> short 't'
        port  = long "port" <> short 'p' <> value 0 <> showDefault

thingy = Right <$> svcParser <|> Left <$> strOption (short 'd')

chkParser = TTL    <$> option text ttl <*> optional (option text notes)
            <|>
            Script <$> option text script <*> option text interval <*> optional (option text notes)
  where ttl      = long "ttl"
        notes    = long "notes"
        script   = long "script" <> short 's'
        interval = long "interval" <> value "10s"

text :: Monad m => String -> m T.Text
text = return . T.pack

main :: IO ()
main = do foo <- execParser (info (helper <*> thingy)
                            fullDesc)
          case foo of
            Right svc -> registerService   (consulPath defaultOpts) svc
            Left name -> deregisterService (consulPath defaultOpts) name
