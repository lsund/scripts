#!/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Applicative  ( (<$>))
import Control.Monad        ( when, return)
import Control.Monad.Extra  ( findM, mapM_)
import Data.Bool            ( Bool(True))
import Data.Int             ( Int)
import Data.List            ( (++), concat, map)
import Data.List.Split      ( splitOn)
import Data.Foldable        ( foldr)
import Data.Map             ( Map, empty, insert, adjust, lookup)
import Data.Maybe           ( Maybe, Maybe(Just, Nothing)
                            , isNothing, fromMaybe)
import Data.Tuple           ( fst)
import Data.Ord             ( Ord)
import GHC.Base             ( Eq, ($), (.))
import GHC.Enum             ( succ)
import System.Directory     ( doesFileExist
                            , createDirectoryIfMissing
                            , listDirectory)
import System.IO            ( IO, FilePath)
import System.Environment   ( getEnv)
import System.Exit          ( exitFailure)


data FileType = File | Directory

warningKB = 5000

splitFirst :: Eq a => [a] -> [a] -> ([a], [a])
splitFirst xs p =
    case splitOn xs p of
        []       -> ([], [])
        [x]      -> (x, [])
        (x : xs) -> (x, concat xs)


compress :: Ord a => [a] -> Map a Int
compress = foldr insertOrIncrement empty
    where insertOrIncrement x m =
            case lookup x m of
              Just _ -> adjust succ x m
              Nothing -> insert x 1 m

baseDir :: FilePath -> FileType -> FilePath
baseDir rootDir File      = rootDir ++ "/trash-files"
baseDir rootDir Directory = rootDir ++ "/trash-dirs"

trashName :: FilePath -> FileType -> Int -> FilePath
trashName path t n = path ++ "-###trashed-" ++ show n

rmCommand :: IO (Maybe FilePath)
rmCommand = findM doesFileExist ["/usr/bin/rm", "/bin/rm"]

trashDir :: IO FilePath
trashDir = (++ "/trash-test/trash") <$> getEnv "HOME"

mkdirs :: [FilePath] -> IO ()
mkdirs = mapM_ (createDirectoryIfMissing True)

countExisting :: FilePath -> FileType -> IO Int
countExisting path t = do
    td <- trashDir
    xs <- listDirectory $ baseDir td t
    let occurences = compress $ map (fst . splitFirst "-###") xs
    return $ fromMaybe 0 (lookup path occurences)

main = do
    rmc <- rmCommand
    td  <- trashDir
    when (isNothing rmc) exitFailure
    mkdirs [baseDir td File, baseDir td Directory]
