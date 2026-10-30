-- # doctests.hs
-- doctest ファイル
--
module Main
    ( main
    ) where

import Test.DocTest

main :: IO ()
main = doctest ["src/Sample/Bonsai.hs"]
