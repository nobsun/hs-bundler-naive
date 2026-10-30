-- # Bundler.CodeReader
--
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Bundler.CodeReader
     where

import Data.Char
import Text.ParserCombinators.ReadP

import Bundler.Types

{- DECL readImpLs [firstParse,rImpLs] ["Text.ParserCombinators.ReadP"] -}
readImpLs :: String -> [EntityName]
readImpLs = firstParse . readP_to_S rImpLs 

{- DECL rParenR [rParenL,rParenR,rName,rComma] ["Text.ParserCombinators.ReadP"] -}
rImpLs :: ReadP [EntityName]
rImpLs = between rParenL rParenR (sepBy rName rComma) <* skipSpaces <* eof

{- DECL rParenL [] ["Text.ParserCombinators.ReadP"] -}
rParenL :: ReadP Char
rParenL = skipSpaces *> char '('

{- DECL rParenR [] ["Text.ParserCombinators.ReadP"] -}
rParenR :: ReadP Char
rParenR = skipSpaces *> char ')'

{- DECL readSegInfo [firstParse,rSegInfo] ["Text.ParserCombinators.ReadP"] -}
{- | SegInfo の読み込み
>>> readSegInfo "{- DECL readSegInfo [firstParse,rSegInfo] [\"Text.ParserCombinators.ReadP\"] -}"
("readSegInfo",["firstParse","rSegInfo"],["Text.ParserCombinators.ReadP"])
-}
readSegInfo :: String -> SegInfo
readSegInfo = firstParse . readP_to_S rSegInfo

{- DECL rSegInfo [SegInfo,rName,rDeps,rImps] ["Text.ParserCombinators.ReadP"] -}
rSegInfo :: ReadP SegInfo
rSegInfo = between (skipSpaces *> string "{- DECL ") (skipSpaces *> string "-}")  ((,,) <$> rName <*> rDeps <*> rImps)
         <* skipSpaces <* eof

{- DECL rName [isIdHead,isIdChar] ["Text.ParserCombinators.ReadP"] -}
rName :: ReadP String
rName = skipSpaces *> ((:) <$> satisfy isIdHead <*> munch isIdChar)

{- DECL isIdHead [] ["Data.Char"] -}
isIdHead :: Char -> Bool
isIdHead c = isAscii c && isAlpha c

{- DECL isIdChar [] ["Data.Char"] -}
isIdChar :: Char -> Bool
isIdChar c = isAscii c && (isAlphaNum c || elem c ("_'" :: String))

{- DECL rDeps [rBrakL,rBrakR,rName,rComma] ["Text.ParserCombinators.ReadP"] -}
rDeps :: ReadP [String]
rDeps = skipSpaces *> between rBrakL rBrakR (sepBy rName rComma)

{- DECL rBrakL [] ["Text.ParserCombinators.ReadP"] -}
rBrakL :: ReadP Char
rBrakL = skipSpaces *> char '['

{- DECL rBrakR [] ["Text.ParserCombinators.ReadP"] -}
rBrakR :: ReadP Char
rBrakR = skipSpaces *> char ']'

{- DECL rImps [rBrakL,rBrakR,rQstr,rComma] ["Text.ParserCombinators.ReadP"] -}
rImps :: ReadP [String]
rImps = skipSpaces *> between rBrakL rBrakR (sepBy rQstr rComma)

{- DECL rQstr [] ["Text.ParserCombinators.ReadP"] -}
rQstr :: ReadP String
rQstr = skipSpaces *> between (char '"') (char '"') (munch ('"' /=))

{- DECL rComma [] ["Text.ParserCombinators.ReadP"] -}
rComma :: ReadP Char
rComma = skipSpaces *> char ','

{- DECL firstParse [] [] -}
firstParse :: [(a, String)] -> a
firstParse rs = case filter (null . snd) rs of
    (x,_):_ -> x
    _       -> error "no parse"
