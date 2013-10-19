module Main where

import Control.Monad
import Data.Foldable        (toList)
import System.Environment
import System.Exit
import System.IO

import Language.BLC.Compile
import Language.BLC.Core

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> compileAndRun file
        _      -> usage

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: blc [INPUT]"
    exitFailure

ensureClosed :: Expr String -> IO ()
ensureClosed e = unless (null vars) $ do
    hPutStrLn stderr "error: unbound variables:"
    mapM_ (hPutStrLn stderr) vars
    exitFailure
  where
    vars = toList e

compileAndRun :: FilePath -> IO ()
compileAndRun file = do
    e <- compileFile file
    ensureClosed e
    runIO e
