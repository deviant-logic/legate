{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (join)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Network.Consul.Http
import           Network.Consul.Types
import           Options.Applicative
import           System.Environment (withArgs)
import           System.Process

import           Legate.Options

main :: IO ()
main = join $ execParser (info (helper <*> subparser commands)
                          (fullDesc <> progDesc "interact with consul") )
          

commands :: Mod CommandFields (IO ())
commands = mconcat [execCommand exec,
                    svcCommand register,
                    checkCommand check,
                    kvCommand kvcmd,
                    helpCommand helpcmd]

withServiceCommand :: ConsulPath -> Register Service -> FilePath -> [String] -> IO ()
withServiceCommand url svc cmd = withService url svc . callProcess cmd

helpcmd :: String -> IO ()
helpcmd cmd = withArgs [cmd, "--help"] main

exec :: CommandOpts Exec -> IO ()
exec CommandOpts {..} = withServiceCommand (consulPath _globalOpts) svc cmd args
  where (Exec svc cmd args) = _commandOpts

register :: CommandOpts (Register Service) -> IO ()
register CommandOpts {..} = registerService (consulPath _globalOpts) _commandOpts

check :: CommandOpts (Register Check) -> IO ()
check CommandOpts {..} = registerCheck (consulPath _globalOpts) _commandOpts

kvcmd :: CommandOpts KV -> IO ()
kvcmd CommandOpts {..} = BL.putStr =<< kv (consulPath _globalOpts) _commandOpts
