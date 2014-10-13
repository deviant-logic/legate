{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (join)
import           Data.Monoid
import           Network.Consul.Http
import           Network.Consul.Types
import           Options.Applicative
import           System.Process

import           Legate.Options

main :: IO ()
main = join $ execParser (info (helper <*> subparser commands)
                          (fullDesc <> progDesc "interact with consul") )
          

commands :: Mod CommandFields (IO ())
commands = mconcat [execCommand exec, svcCommand register]

withServiceCommand :: String -> Service -> String -> IO ()
withServiceCommand url svc = withService url svc . callCommand

exec :: CommandOpts Exec -> IO ()
exec CommandOpts {..} = withServiceCommand (consulPath _globalOpts) svc cmd
  where (Exec svc cmd) = _commandOpts

register :: CommandOpts (Register Service) -> IO ()
register CommandOpts {..} = registerService (consulPath _globalOpts) _commandOpts
