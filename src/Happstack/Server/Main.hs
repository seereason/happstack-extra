{-# OPTIONS_GHC -Wwarn -fwarn-missing-signatures -fwarn-unused-imports -fwarn-unused-binds #-}
module Happstack.Server.Main
    ( main
    ) where

import Control.Concurrent (MVar, forkIO, killThread)
import Control.Monad (liftM)
import Happstack.Server (Conf(..), nullConf, simpleHTTP, wdgHTMLValidator, ToMessage, ServerPartT)
import Happstack.State.Control(waitForTermination)
import Happstack.Util.Cron(cron)
import Happstack.State (Proxy(..), Saver(Queue, FileSaver), createCheckpoint, runTxSystem, shutdownSystem, Methods, Component, TxControl)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)

main :: (ToMessage a, Methods st, Component st) => String -> Proxy st -> (FilePath -> ServerPartT IO a) -> IO ()
main name entryPoint impl = do
  eConf <- liftM (parseConfig' name) getArgs
  let conf' =
          case eConf of
            Left e -> error (unlines e)
            Right c -> c
  control <- startSystemState' (store conf') entryPoint
  tid <- forkIO $ simpleHTTP (conf conf') (impl $ static conf')
  cronId <- forkIO $ cron (60*60*24) (createCheckpoint control)
  putStrLn ("running on port " ++ show (port (conf conf')) ++ "...")
  waitForTermination
  killThread tid
  killThread cronId
  createCheckpoint control
  shutdownSystem control

parseConfig' :: String -> [String] -> Either [String] Conf'
parseConfig' name args
    = case getOpt Permute ho args of
        (flags,_,[]) -> Right $ foldr ($) (standardConf name 8001) flags
        (_,_,errs)   -> Left errs

ho :: [OptDescr (Conf' -> Conf')]
ho = [ Option [] ["http-port"] (ReqArg (\h c -> c { conf = (conf c) {port = read h} }) "port") "port to bind http server"
     , Option [] ["no-validate"] (NoArg (\ c -> c { conf = (conf c) { validator = Nothing } })) "Turn off HTML validation"
     , Option [] ["validate"] (NoArg (\ c -> c { conf = (conf c) { validator = Just wdgHTMLValidator } })) "Turn on HTML validation"
     , Option [] ["store"] (ReqArg (\h c -> c {store = h}) "PATH") "The directory used for database storage."
     , Option [] ["static"] (ReqArg (\h c -> c {static = h}) "PATH") "The directory searched for static files" ]

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' path proxy =
    runTxSystem (Queue (FileSaver path)) proxy

data Conf'
    = Conf' { conf :: Conf
            , app :: String
            , store :: FilePath
            , static :: FilePath }

standardConf :: String -> Int -> Conf'
standardConf appName defaultPort
    = Conf' { conf = nullConf {validator = Just wdgHTMLValidator, port = defaultPort}
            , app = appName
            , store = "_local/" ++ appName ++ "_state"
            , static = "static" }
