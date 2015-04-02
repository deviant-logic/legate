{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Legate.Options where

import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.String
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


fstr :: IsString a => ReadM a
fstr = fromString <$> str


registrator :: String -> Parser a -> Parser (Register a)
registrator thing p = Register <$> strOption name <*> optional (strOption thingid) <*> p
                      <|> DeRegister <$> strOption dereg
  where dereg = short 'd' <> help "name to deregister"
        thingid = long "id"   <> short 'i'
                  <> help ("identifier for this " ++ thing ++ ", if different from name")
                  <> metavar "ID"
        name    = long "name" <> short 'n' <> help ("name for this " ++ "thing")
                  <> metavar "NAME"

svcParser :: Parser Service
svcParser = Service <$> many (option fstr tag)
                    <*> option auto port
                    <*> optional (strOption address)
                    <*> optional chkParser
  where tag     = long "tag"  <> short 't'
                  <> metavar "TAG" <> help "tags for this service on this node"
        port    = long "port" <> short 'p' <> value 0
                  <> showDefault <> help "port this service runs on"
                  <> metavar "PORT"
        address = long "address" <> short 'a'
                  <> metavar "ADDRESS" <> help "specify a service-specific address"

chkParser :: Parser Check
chkParser = TTL    <$> option fstr ttl <*> optional (option fstr notes)
            <|> Script <$> option fstr script
                       <*> option fstr interval
                       <*> optional (option fstr notes)
  where ttl      = long "ttl" <> help "time to live check duration"
                   <> metavar "DURATION"
        notes    = long "notes" <> help "human readable description of this check"
                   <> metavar "NOTES"
        script   = long "script" <> short 's' <> help "script to run for this check"
                   <> metavar "COMMAND"
        interval = long "interval" <> value "10s"
                   <> help "check interval for this script check"
                   <> showDefault <> metavar "DURATION"

type Command a = (CommandOpts a -> IO ()) -> Mod CommandFields (IO ())

commander :: String -> String -> Parser a -> Command a
commander cmd desc p f = command cmd (info (helper <*> fmap f (commandOptsParser p))
                                      (progDesc desc))

helpCommand :: (String -> IO ()) -> Mod CommandFields (IO ())
helpCommand f = command "help" (info (helper <*> (f <$> p))
                                (progDesc "get help for a command"))
  where p = strArgument $ metavar "COMMAND"

svcCommand :: Command (Register Service)
svcCommand = commander "service" "register or deregister a service"
                     $ registrator "service" svcParser

data Exec = Exec (Register Service) FilePath [String]

exParser :: Parser Exec
exParser = Exec <$> registrator "service" svcParser
                <*> strArgument cmd
                <*> many (strArgument arg)
  where cmd = help "command to run"   <> metavar "COMMAND"
        arg = help "command argument" <> metavar "ARGS"

execCommand :: Command Exec
execCommand = commander "exec" "run a command wrapped in service registration" exParser

checkCommand :: Command (Register Check)
checkCommand = commander "check" "register or deregister a check"
                       $ registrator "check" chkParser

kvParser :: Parser KV
kvParser = GetKey     <$> strArgument key
           <|> PutKey <$> strOption set <*> option fstr val
           <|> DelKey <$> strOption   del
  where key =                               metavar "KEY"   <> help "key to get"
        set = long "key"    <> short 'k' <> metavar "KEY"   <> help "key to set"
        del = long "delete" <> short 'd' <> metavar "KEY"   <> help "key to delete"
        val = long "set"    <> short 's' <> metavar "VALUE" <> help "value to set"

kvCommand :: Command KV
kvCommand = commander "kv" "get or set values in the key/value store" kvParser
