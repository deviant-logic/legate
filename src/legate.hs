{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad (join)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Network.Consul.Http
import           Network.Consul.Types
import qualified Network.HTTP.Client as HTTP
import           Network.Wreq.Session
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

makeManagerSettings :: ConsulPath -> HTTP.ManagerSettings
makeManagerSettings (UnixPath p) = HTTP.defaultManagerSettings { HTTP.managerRawConnection = return $ \_ _ _ -> makeUnixSocketConnection p 4096 }
makeManagerSettings _            = HTTP.defaultManagerSettings

withServiceCommand :: ConsulPath -> Register Service -> FilePath -> [String] -> IO ()
withServiceCommand url svc cmd args = withSessionWith (makeManagerSettings url) $ \s -> withService s url svc $ callProcess cmd args

helpcmd :: String -> IO ()
helpcmd cmd = withArgs [cmd, "--help"] main

exec :: CommandOpts Exec -> IO ()
exec CommandOpts {..} = withServiceCommand (consulPath _globalOpts) svc cmd args
  where (Exec svc cmd args) = _commandOpts

register :: CommandOpts (Register Service) -> IO ()
register CommandOpts {..} = withSessionWith (makeManagerSettings url) $ \s -> registerService s url _commandOpts
  where
    url = consulPath _globalOpts

check :: CommandOpts (Register Check) -> IO ()
check CommandOpts {..} = withSessionWith (makeManagerSettings url) $ \s -> registerCheck s url _commandOpts
  where
    url = consulPath _globalOpts


kvcmd :: CommandOpts KV -> IO ()
kvcmd CommandOpts {..} = BL.putStr =<< (withSessionWith (makeManagerSettings url) $ \s -> kv s url _commandOpts)
  where
    url = consulPath _globalOpts
