-- # Bundler.Bundle
--
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
module Bundler.Bundle
    where

import Data.List
import Bundler.CodeReader
import Bundler.CodeSegTable (moduleToCodeSegTable)
import Bundler.CollectCode

bundle :: (String, String) -> String -> String
bundle (name, src) target
    = case break (isPrefixOf ("import " ++ name)) (lines target) of
        (as,b:bs) -> case break (isPrefixOf "{- Bonsai code -}") bs of
            (cs,[])   -> unlines (as ++ imps ++ cs ++ cds)
            (cs,d:ds) -> unlines (as ++ imps ++ cs ++ d:cds ++ ds)
            where
                impls = readImpLs $ dropWhile ('(' /=) b
                tbl   = moduleToCodeSegTable src
                (imps, cds) = genCodes (collectCode tbl impls)
        _         -> error "bundle: not found bundle source"
