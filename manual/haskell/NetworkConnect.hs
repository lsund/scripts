{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding   (strip, drop, takeWhile, init)

import System.Process
import System.Directory (listDirectory, doesFileExist, doesPathExist)

import Data.Text        (Text, strip, pack, drop, takeWhile, init)
import Data.List        (nub)

import Control.Monad    (filterM)

getESSID x = init $ takeWhile (/= '\\') (drop 7 x)

isRegularFile :: FilePath -> IO Bool
isRegularFile fp = do
    exists <- doesPathExist fp
    if exists then
        doesFileExist fp
    else
        return False

networkScan :: IO [Text]
networkScan = do
    res <- readProcess "grep" ["ESSID"] =<<
           readProcess "sudo" ["-S", "iwlist", "wlp58s0", "scan"] ""
    return $ map (getESSID . strip . pack) (lines res)


-- readFiles :: FilePath -> IO [FilePath]
readFiles fp = do
    files <- listDirectory "/etc/netctl"
    let absoluteFiles = map ("/etc/netctl/" ++) files
        essid fp      = readProcess "sudo" ["-S", "cat", fp] ""
    regularFiles <- filterM isRegularFile absoluteFiles
    contents     <- mapM essid regularFiles
    return contents


main :: IO ()
main = do
    putStrLn "Available ids:"
    ids <- networkScan
    mapM_ print (nub ids)
    return ()
