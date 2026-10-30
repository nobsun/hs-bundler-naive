{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main
    ( main
    ) where

import Data.List.Split
import System.Environment
import System.FilePath
import Text.Printf
import Bundler.Bundle

main :: IO ()
main = do
    { prog <- getProgName
    ; args <- getArgs
    ; case args of
        srcLib:targetPath:_ 
            -> do
                { src <- readFile ("src" </> toPath srcLib <.> "hs")
                ; tgt <- readFile targetPath
                ; putStr $ bundle (srcLib, src) tgt
                }
        _   -> usage prog
    ; return ()
    }

toPath :: String -> FilePath
toPath s = foldr1 (</>) (splitOn "." s)

usage :: String -> IO ()
usage = printf "Usage: %s <source module> <target path>\n"