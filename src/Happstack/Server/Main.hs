{-# OPTIONS_GHC -Wwarn -fwarn-missing-signatures -fwarn-unused-imports -fwarn-unused-binds #-}
module Happstack.Server.Main
    ( main
    ) where

import Control.Concurrent (MVar, forkIO, killThread)
import Happstack.Data.Logger (LogMode(..), setupLogger)
import Happstack.Server (Conf(..), nullConf, simpleHTTP, wdgHTMLValidator, ToMessage, ServerPartT)
import Happstack.State.Control(waitForTermination)
import Happstack.Util.Cron(cron)
import Happstack.State (Proxy(..), Saver(Queue, FileSaver), createCheckpoint, runTxSystem, shutdownSystem, Methods, Component, TxControl)
import System.Environment (getArgs)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)

main :: (ToMessage a, Methods st, Component st) => String -> Proxy st -> (FilePath -> ServerPartT IO a) -> IO ()
main name entryPoint impl = do
  conf' <- parseConfig >>= either (error . unlines) (\ f -> return . f $ standardConf name 8001)
  setupLogger name (logs conf') (logMode conf')
  control <- startSystemState' (store conf') entryPoint
  tid <- forkIO $ simpleHTTP (conf conf') (impl $ static conf')
  cronId <- forkIO $ cron (60*60*24) (createCheckpoint control)
  putStrLn ("running on port " ++ show (port (conf conf')) ++ "...")
  waitForTermination
  killThread tid
  killThread cronId
  createCheckpoint control
  shutdownSystem control

-- |An improved version of Happstack.Server.SimpleHTTP.parseConfig, which
-- allows the caller to supply a list of Option.
parseConfig :: IO (Either [String] (Conf' -> Conf'))
parseConfig = do
  args <- getArgs
  case getOpt Permute opts args of
    (flags,_,[]) -> return . Right $ \ appConf -> foldr ($) appConf flags
    (_,_,errs)   -> return . Left $ errs

opts :: [OptDescr (Conf' -> Conf')]
opts =
    [ Option [] ["http-port"] (ReqArg (\h c -> c {conf = (conf c) {port = read h}}) "port") "port to bind http server"
    , Option [] ["no-validate"] (NoArg (\ c -> c {conf = (conf c) {validator = Nothing}})) "Turn off HTML validation"
    , Option [] ["validate"] (NoArg (\ c -> c {conf = (conf c) {validator = Just wdgHTMLValidator}})) "Turn on HTML validation"
    , Option [] ["store"] (ReqArg (\h c -> c {store = h}) "PATH") "The directory used for database storage."
    , Option [] ["static"] (ReqArg (\h c -> c {static = h}) "PATH") "The directory searched for static files"
    , Option [] ["logs"]        (ReqArg (\h c -> c {logs = h}) "PATH") "The directory to store log files in"
    , Option [] ["log-mode"]    (ReqArg (\h c -> c {logMode = read h}) (show ([minBound .. maxBound] :: [LogMode]))) "The logging mode to use" ]

data Conf'
    = Conf' { conf :: Conf
            , app :: String
            , store :: FilePath
            , static :: FilePath
            , logs :: FilePath
            , logMode :: LogMode }

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' path proxy =
    runTxSystem (Queue (FileSaver path)) proxy

standardConf :: String -> Int -> Conf'
standardConf appName defaultPort
    = Conf' { conf = nullConf {validator = Just wdgHTMLValidator, port = defaultPort}
            , app = appName
            , store = "_local/" ++ appName ++ "_state"
            , static = "static"
            , logs = ""
            , logMode = Development }
