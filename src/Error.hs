module Error
    ( errorExit,
      successExit,
      errorExits,
      errorExiti,
      errorExitf,
    ) where

import Text.Read
import Control.Exception
import System.Exit

errorExit :: IO ()
errorExit = exitWith $ ExitFailure 84

errorExits :: IO String
errorExits = exitWith $ ExitFailure 84

errorExiti :: IO Int
errorExiti = exitWith $ ExitFailure 84

errorExitf :: IO Float
errorExitf = exitWith $ ExitFailure 84

successExit :: IO ()
successExit = exitSuccess