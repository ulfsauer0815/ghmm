module LogFormatter
    ( simpleColorLogFormatter
    , simpleLogFormatter

    , getSimpleLogFormatter

    , withFormatter
    ) where

import           System.Console.ANSI
import           System.IO
import           System.Log
import           System.Log.Formatter
import           System.Log.Handler

import           Data.Monoid

-- ----------------------------------------------

-- | A simple log formatter which colors the output on priority basis.
simpleColorLogFormatter :: String -> a -> LogRecord -> String -> IO String
simpleColorLogFormatter format a r@(prio, msg) =
  (simpleLogFormatter $
        setSGRCode ansiFormat
     <> format
     <> setSGRCode [Reset]
  ) a r
  where
  ansiFormat = case prio of
    WARNING   -> [SetColor Foreground Dull Yellow]
    ERROR     -> [SetColor Foreground Dull Red]
    CRITICAL  -> [SetColor Foreground Vivid Red]
    ALERT     -> [SetColor Foreground Vivid Red]
    EMERGENCY -> [SetColor Foreground Vivid Red]
    _         -> []


-- | Helper to add a formatter to a LogHandler.
withFormatter :: (Monad m, LogHandler r) => m r -> LogFormatter r -> m r
withFormatter h f = fmap (`setFormatter` f) h


-- | Returns a simple log formatter, which is colored if the handle supports ANSI.
--
--   The current implementation checks that the handle is a terminal, and that
--   the TERM environment variable doesn't say dumb (which is what Emacs sets
--   for its own terminal).
getSimpleLogFormatter :: Handle -> IO (String -> a -> LogRecord -> String -> IO String)
getSimpleLogFormatter handle = do
  ansiSupported <- hSupportsANSI handle
  if ansiSupported
  then return simpleColorLogFormatter
  else return simpleLogFormatter
