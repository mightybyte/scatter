{-|

Copies a file to multiple locations, eliminating multiple reads of the source
file.

-}
module Main where

import System
import System.IO
import System.Exit
import System.FilePath.Posix

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Enumerator as E
import Data.Enumerator.Binary (enumHandle)

scatter :: [Handle] -> Int
     -> E.Stream ByteString
     -> E.Iteratee ByteString IO Int
scatter hs bytes E.EOF = E.yield bytes E.EOF
scatter hs bytes (E.Chunks cs) = E.Iteratee $ do
    let !bs = B.concat cs
    mapM_ (\h -> B.hPut h bs >> hFlush h) hs
    return $ E.Continue $ scatter hs (bytes+B.length bs)

usage = do
    putStrLn $ unlines
        ["Usage:",""
        ,"scatter <source_file> <destinations>"
        ,"  source_file - The name of the file to copy"
        ,"  destinations - One or more destination directories to copy to"
        ]
    exitFailure

------------------------------------------------------------------------------
main = do
    args <- getArgs
    when (length args < 2) usage
    let src = head args
    hSrc <- openBinaryFile src ReadMode
    let openOut f = openBinaryFile (f </> takeFileName src) WriteMode
    hs <- mapM openOut $ tail args
    res <- E.run $ enumHandle 4096 hSrc $ E.Continue $ scatter hs 0
    putStrLn $ either err (good $ length $ tail args) res
  where
    err e = "Error: "++show e
    good files bytes = show bytes ++ " bytes copied to " ++ show files ++ " locations"
