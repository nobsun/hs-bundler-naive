-- # Bundler.CodeSegTable
--
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Bundler.CodeSegTable
    ( CodeSegTable
    , moduleToCodeSegTable
    ) where

import Data.Map qualified as M
import Data.List

import Bundler.CodeReader
import Bundler.Types

{- DECL moduleToCodeSegTable [toCodeSegEnt, toCodeSegs] ["Data.Map qualified as M","Bundler.Types"] -}
moduleToCodeSegTable :: String -> CodeSegTable
moduleToCodeSegTable = M.fromList . map toCodeSegEnt . toCodeSegs

{- DECL toCodeSegEnt [CodeSeg,CodeSegEnt] [] -}
toCodeSegEnt :: CodeSeg -> CodeSegEnt
toCodeSegEnt = \ case
    ((name,deps,imps),descs,codes) -> (name,((deps,imps),(descs,codes)))

{- DECL toCodeSegs [fromRaw,toRawCodeSegs] [] -}
toCodeSegs :: String -> [CodeSeg]
toCodeSegs = map fromRaw . toRawCodeSegs . lines

{- DECL fromRaw [RawCodeSeg,CodeSeg,readSeginfo] [] -}
fromRaw :: RawCodeSeg -> CodeSeg
fromRaw (s,ds,cs) = (readSegInfo s, ds, cs)

{- DECL toRawCodeSegs [] ["Data.List"] -}
toRawCodeSegs :: [String] -> [RawCodeSeg]
toRawCodeSegs = unfoldr psi
    where
        psi lls = case dropWhile (not . isPrefixOf "{- DECL ") lls of
            l:ls   -> case ls of
                t:_ | "{- | " `isPrefixOf` t -> case break (isPrefixOf "-}") ls of
                    (us,v:vs)                    -> case break (isPrefixOf "{- DECL ") vs of
                        (ws,xs)                      -> Just ((l,us++[v],ws), xs)
                    _                            -> error "moduleToRawCodeSegs.psi: unexpected eof"
                    | otherwise              -> case break (isPrefixOf "{- DECL ") ls of
                        (ws,xs)                  -> Just ((l,[],ws), xs)
                _                            -> error "moduleToRawCodeSegs.psi: unexpected eof"
            _      -> Nothing

