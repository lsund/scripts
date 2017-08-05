#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Text as T (Text, words, lines, append)
import Data.Text.IO as TIO
import Filesystem.Path.CurrentOS (fromText, decodeString)

homedir = "/home/lsund/"
confdir = "/home/lsund/Scripts/config/"
bufdir = "/home/lsund/Scripts/data/buffer"

main = do
    content <- TIO.readFile "data/config-paths"
    let contentSplit = map T.words $  T.lines content
    let added = map (\[x, y] -> [T.append confdir x, T.append homedir y]) contentSplit
    let equalsM = map (\entry -> inproc "comp" entry empty) added
    equals <- mapM strict equalsM
    let notequals = map fst $ filter ((== "Not Equal\n") . snd) $ zip contentSplit equals
    mapM_ print notequals
