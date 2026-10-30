{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Data.ByteString.Char8 qualified as B
import Data.Maybe
import Debug.Trace qualified as Debug

import Sample.Bonsai (msFromList, msSize, yesno)

_DEBUG :: Bool
_DEBUG = () /= ()

type I = Int
type O = String

type Dom = [I]
type Codom = O

type Solver = Dom -> Codom

solve :: Solver
solve = \ case
    as -> yesno $ length as == msSize (msFromList as)

decode :: [[I]] -> Dom
decode = \ case
    as:_ -> as
    _    -> error "decode: invalid input"

encode :: Codom -> [[O]]
encode = \ case
    r -> _trace "debug print on" [[r]]

{- Bonsai code -}

{- Common code -}
main :: IO ()
main = B.interact (detokenize . encode . solve . decode . entokenize)

entokenize :: B.ByteString -> [[I]]
entokenize = map (map toToken . B.words). B.lines

detokenize :: [[O]] -> B.ByteString
detokenize = B.unlines . map (B.unwords . map fromToken)

class AsToken t where
    toToken   :: B.ByteString -> t
    fromToken :: t -> B.ByteString

instance AsToken B.ByteString where
    toToken :: B.ByteString -> B.ByteString
    toToken = id
    fromToken :: B.ByteString -> B.ByteString
    fromToken = id

instance AsToken String where
    toToken :: B.ByteString -> String
    toToken = B.unpack
    fromToken :: String -> B.ByteString
    fromToken = B.pack

instance AsToken Int where
    toToken :: B.ByteString -> Int
    toToken = fst . fromJust . B.readInt
    fromToken :: Int -> B.ByteString
    fromToken = B.pack . show

instance AsToken Integer where
    toToken :: B.ByteString -> Integer
    toToken = fst . fromJust . B.readInteger
    fromToken :: Integer -> B.ByteString
    fromToken = B.pack . show

instance AsToken Double where
    toToken :: B.ByteString -> Double
    toToken = read . B.unpack
    fromToken :: Double -> B.ByteString
    fromToken = B.pack . show

_trace :: String -> a -> a
_trace = \ case
    s | _DEBUG    -> Debug.trace s
      | otherwise -> id

_tracing :: Show a => a -> a
_tracing = _trace . show <*> id
