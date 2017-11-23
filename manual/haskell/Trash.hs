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
import GHC.Show             ( show, Show)
import System.Directory     ( doesFileExist
                            , doesDirectoryExist
                            , createDirectoryIfMissing
                            , listDirectory
                            , renamePath
                            , getCurrentDirectory)
import System.IO            ( IO, FilePath, putStrLn)
import System.Environment   ( getEnv, getArgs)
import System.Exit          ( exitFailure)
import System.FilePath      ( takeFileName)


data FileType = File | Directory deriving (Show)

data Config = Config { trashDir :: FilePath
                     , workDir  :: FilePath
                     , rmScript :: Maybe FilePath
                     }


-- TODO import datetime, Data.DateTime and put getCurrentTime into filename
-- TODO use remove when inside trashdir


----------------------------------------------------------------------------
-- Hard Coded Config


warningKB = 5000         :: Int
baseTrashDir = "/.trash" :: FilePath


----------------------------------------------------------------------------
-- Pure


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


joinAbsolutePath :: FilePath -> FilePath -> FilePath
joinAbsolutePath path = ((path ++ "/") ++) . takeFileName


trashName :: FilePath -> FileType -> Int -> FilePath
trashName path t n = path ++ "-###trashed-" ++ show n


getRmCommand :: IO (Maybe FilePath)
getRmCommand = findM doesFileExist ["/usr/bin/rm", "/bin/rm"]


----------------------------------------------------------------------------
-- IO


mkdirs :: [FilePath] -> IO ()
mkdirs = mapM_ (createDirectoryIfMissing True)


loadConfig :: IO Config
loadConfig = do
    td   <- getTrashDir
    wd   <- getCurrentDirectory
    rmc  <- getRmCommand
    return $ Config td wd rmc


getTrashDir :: IO FilePath
getTrashDir = (++ baseTrashDir) <$> getEnv "HOME"


fileType :: FilePath -> IO (Maybe FileType)
fileType fp = do
    dir  <- doesDirectoryExist fp
    file <- doesFileExist fp
    return $
        if dir
            then Just Directory
            else if file then Just File else Nothing


countExisting :: Config -> FilePath -> FileType -> IO Int
countExisting conf path t = do
    xs <- listDirectory $ baseDir (trashDir conf) t
    let occurences = compress $ map (fst . splitFirst "-###") xs
    return $ fromMaybe 0 (lookup path occurences)


makeTrashName ::  Config -> FilePath -> FileType -> IO FilePath
makeTrashName conf path t = do
    let basename = takeFileName path
    fname  <- trashName basename t  . succ <$> countExisting conf basename t
    return $ baseDir (trashDir conf) t ++ "/" ++ fname


moveToTrash :: Config -> FilePath -> IO ()
moveToTrash conf path = do
    mt <- fileType path
    case mt of
        Just t -> do
            destName <- makeTrashName conf path t
            renamePath path destName
            putStrLn $ "Moved " ++ path ++ " to trash."
        Nothing ->
            putStrLn $ "No such file or directory: " ++ path


main :: IO ()
main = do
    args <- getArgs
    conf <- loadConfig
    let td = trashDir conf
        wd = workDir conf
        rms = rmScript conf
    when (isNothing rms) exitFailure
    mkdirs [baseDir td File, baseDir td Directory]
    let paths = map (joinAbsolutePath wd) args
    mapM_ (moveToTrash conf) paths

