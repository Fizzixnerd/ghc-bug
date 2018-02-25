{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Functor
import System.Directory
import GHC
import GHC.Paths
import DynFlags
import Unsafe.Coerce

loadPlugin :: String -> String -> String -> IO a
loadPlugin dir modName value = do
  withCurrentDirectory dir $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ withCleanupSession $ do

    dynFlags <- getSessionDynFlags
    setSessionDynFlags $ dynamicTooMkDynamicDynFlags $ dynFlags 
      { importPaths = [modName] ++ importPaths dynFlags
      , hscTarget = HscAsm
      , ghcLink = LinkInMemory
      , ghcMode = CompManager
      }
    sequence [guessTarget modName Nothing] >>= setTargets
    load LoadAllTargets
    setContext [IIDecl $ simpleImportDecl $ mkModuleName modName]
    fetched <- compileExpr (modName ++ "." ++ value)
    return (unsafeCoerce fetched :: a)

anotherFunc :: IO ()
anotherFunc = do
  val :: String <- loadPlugin "scripts/" "Mod" "val"
  print val

someFunc :: IO ()
someFunc = do
  val1 :: String <- loadPlugin "scripts/" "Mod" "val"
  val2 :: String <- loadPlugin "scripts/" "Mod" "val"
  print val1
  print val2
