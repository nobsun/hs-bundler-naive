-- # Bundler.Types
--
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
module Bundler.Types
    where

import Data.Map qualified as M

{- DECL EntityName [] [] -}
{- | Entity name
-}
type EntityName = String

{- DECL CodeLine [] [] -}
{- | Code line
-}
type CodePart = [String]

{- DECL DescPart [] [] -}
{-  | Description part
-}
type DescPart = [String]

{- DECL ImportLine [] [] -}
{- | Import declaration without "import" keyword
-}
type ImportLine = String

{- DECL CodeSegTable [EntityName,ImportLine,DescPart,CodePart] ["Data.Map qualified as M"] -}
{- | Code segment table
-}
type CodeSegTable = M.Map EntityName (([EntityName],[ImportLine]),(DescPart,CodePart))

type CodeSegEnt
    = ( EntityName
      , ( ([EntityName], [ImportLine])
        , (DescPart, CodePart)
        )
      )

{- DECL SegInfo [] [] -}
type SegInfo
    = ( EntityName
      , [EntityName] -- deps
      , [ImportLine] -- deps (import)
      )

{- DECL CodeSeg [SegInfo] [] -}
type CodeSeg
    = ( SegInfo
      , DescPart
      , CodePart
      )

{- DECL RawCodeSeg [] [] -}
type RawCodeSeg
    = ( String   -- DECL line
      , DescPart -- description lines
      , CodePart -- code
      )
