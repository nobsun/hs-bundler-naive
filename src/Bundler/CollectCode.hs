-- # Bundler.CollectCode
--
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Bundler.CollectCode
    ( collectCode
    , genCodes
    ) where

import Data.List
import Data.Map qualified as M
import Data.Set qualified as S
import Bundler.CodeSegTable
import Bundler.Types

{- DECL collectCode [] ["Bundler.Types","Bundler.CodeSegTable","Data.Map qualified as M","Data.Set qualified as S"] -}
{- | collects import declarations and entity definitions
-}
collectCode :: CodeSegTable -> ([EntityName],[ImportLine]) -> (S.Set ImportLine, M.Map EntityName CodePart)
collectCode tbl (es0,imps0) = iter S.empty S.empty M.empty es0
    where
        
        iter ents imps csgs = \ case
            []   -> (imps `S.difference` S.fromList imps0, csgs)
            e:es | S.member e ents -> iter ents imps csgs es
                 | otherwise -> case tbl M.!? e of
                    Nothing      -> error "collectCode: not in CodeSegTable"
                    Just ((edps,idps),(_descls,codels))
                                 -> iter (S.insert e ents) 
                                         (foldl' (flip S.insert) imps idps)
                                         (M.insert e codels csgs)
                                         (edps ++ es)

{- DECL  [] ["Data.Map qualified as M","Data.Set qualified as S"] -}
{- | generates code lines for import declarations and for entity definitions
-}
genCodes :: (S.Set ImportLine, M.Map EntityName CodePart) -> ([String], [String])
genCodes (ims,cdss) = (map ("import " ++) $ S.toList ims, concat (M.elems cdss))

_test :: IO ()
_test = do
    { testmod <- readFile "/home/nobsun/proj/hs-bundler-naive/src/Sample/Bonsai.hs"
    ; let testtbl = moduleToCodeSegTable testmod
    ; let (imls,cdls) = genCodes $ collectCode testtbl (["msFromList","msToList"],["Data.List"])
    ; putStr $ unlines $ imls ++ "" : cdls
    }