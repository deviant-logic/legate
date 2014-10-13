{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Legate.Options where

import qualified Data.Text as T
import           Network.Consul.Http
import           Network.Consul.Types
import           Options.Applicative

data CommandOpts a = CommandOpts {
  _globalOpts :: GlobalOpts,
  _commandOpts :: a
}

commandOptsParser :: Parser a -> Parser (CommandOpts a)
commandOptsParser pa = CommandOpts <$> globalOptsParser <*> pa

data GlobalOpts = GlobalOpts {
  _consulHost :: String,
  _consulPort :: Int,
  _consulSSL  :: Bool
}

defaultOpts :: GlobalOpts
defaultOpts = GlobalOpts "localhost" 8500 False

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts <$> strOption host <*> option auto port <*> switch ssl
  where GlobalOpts {..} = defaultOpts
        host = long "consul-host" <> short 'H' <> value _consulHost <> showDefault
               <> help "host running consul" <> metavar "HOST"
        port = long "consul-port" <> short 'P' <> value _consulPort <> showDefault
               <> help "consul HTTP API port on HOST" <> metavar "PORT"
        ssl  = long "ssl" <> showDefault <> help "use https"

consulPath :: GlobalOpts -> String
consulPath (GlobalOpts host port ssl) = scheme ++ host ++ ":" ++ show port
  where scheme | ssl       = "https://"
               | otherwise = "http://"

text :: Monad m => String -> m T.Text
text = return . T.pack

registrator :: Parser a -> Parser (Register a)
registrator p = Register <$> p
                <|> DeRegister <$> strOption mods
  where mods = short 'd' <> help "name to deregister"

svcParser :: Parser Service
svcParser = Service <$> optional (option text svcid)
                    <*> option text name
                    <*> many (option text tag)
                    <*> option auto port
                    <*> optional chkParser
  where svcid = long "id"   <> short 'i' <> help "identifier for this service, if different from name"
        name  = long "name" <> short 'n' <> help "name for this service"
        tag   = long "tag"  <> short 't' <> help "tags for this service on this node"
        port  = long "port" <> short 'p' <> value 0 <> showDefault <> help "port this service runs on"

chkParser :: Parser Check
chkParser = TTL    <$> option text ttl <*> optional (option text notes)
            <|>
            Script <$> option text script <*> option text interval <*> optional (option text notes)
  where ttl      = long "ttl" <> help "time to live check duration"
        notes    = long "notes" <> help "human readable description of this check"
        script   = long "script" <> short 's' <> help "script to run for this check"
        interval = long "interval" <> value "10s" <> help "check interval for this script check"
                   <> showDefault

type Command a = (CommandOpts a -> IO ()) -> Mod CommandFields (IO ())

commander :: String -> String -> Parser a -> Command a
commander cmd desc p f = command cmd (info (helper <*> fmap f (commandOptsParser p))
                                      (progDesc desc))

svcCommand :: Command (Register Service)
svcCommand = commander "service" "register or deregister a service" (registrator svcParser)

data Exec = Exec Service String

exParser :: Parser Exec
exParser = Exec <$> svcParser <*> strOption cmd
  where cmd = long "command" <> short 'e' <> help "command to run"

execCommand :: Command Exec
execCommand = commander "exec" "run a command wrapped in service registration" exParser
