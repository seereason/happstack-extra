module Happstack.Data.Logger (LogMode(..),setupLogger) where

import System.Log.Logger
    ( Priority(..)
    , rootLoggerName
    , setLevel
    , setHandlers
    , updateGlobalLogger
    )
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.FilePath ((</>))
import System.IO (stdout)

data LogMode
    = Production
    | Development
      deriving (Read, Show, Eq, Ord, Enum, Bounded)

setupLogger :: String -> FilePath -> LogMode -> IO ()
setupLogger progName logDir logMode = do
    appLog    <- fileHandler (logDir </> (progName ++ "_root.log"))   DEBUG
    accessLog <- fileHandler (logDir </> (progName ++ "_access.log")) INFO
    stdoutLog <- streamHandler stdout DEBUG

    case logMode of
      Development -> do
          -- Root Log
          updateGlobalLogger rootLoggerName 
            (setLevel DEBUG . setHandlers [appLog, stdoutLog])
          -- Access Log
          updateGlobalLogger "Happstack.Server.AccessLog.Combined" 
            (setLevel INFO . setHandlers [accessLog])

      Production -> do
          -- Root Log
          updateGlobalLogger rootLoggerName 
            (setLevel INFO . setHandlers [appLog])
          -- Access Log
          updateGlobalLogger "Happstack.Server.AccessLog.Combined" 
            (setLevel INFO . setHandlers [accessLog])
