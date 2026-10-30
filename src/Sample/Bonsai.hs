-- # Sample.Bonsai
-- 
-- ## 言語拡張と`module`宣言
-- 
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Sample.Bonsai
      where

import Data.Array
import Data.Bool
import Data.List
import Data.Map qualified as M
import Data.String

{- DECL RLE [] [] -}
{- | Run-Length-Encoded
-}
type RLE a = [(a, Int)]

{- DECL toRLE [RLE] [] -}
{- | encodes to RLE
>>> sampleStr = "abbbccdddd"
>>> toRLE sampleStr
[('a',1),('b',3),('c',2),('d',4)]
-}
toRLE :: Eq a
      => [a] -> RLE a
toRLE = \ case
    x:xs -> iter 1 x xs
    _    -> []
    where
        iter !c y = \ case
            z:zs | y == z    -> iter (succ c) y zs
                 | otherwise -> (y,c) : iter 1 z zs
            _                -> [(y,c)]

{- DECL fromRLE [RLE] [] -}
{- | decodes from RLE
>>> sampleStr = "abbbccdddd"
>>> sampleRLE = toRLE sampleStr
>>> sampleRLE
[('a',1),('b',3),('c',2),('d',4)]
>>> fromRLE sampleRLE
"abbbccdddd"
>>> fromRLE (toRLE sampleStr) == sampleStr
True
-}
fromRLE :: RLE a -> [a]
fromRLE = concatMap phi
    where
        phi (x,c) = replicate c x

{- DECL splitAtRLE [RLE] [] -}
{- | makes splitAt on RLE
>>> sampleStr = "abbbccdddd"
>>> sampleStr
"abbbccdddd"
>>> sampleRLE = toRLE sampleStr
>>> sampleRLE
[('a',1),('b',3),('c',2),('d',4)]
>>> splitAtRLE 3 sampleRLE
([('a',1),('b',2)],[('b',1),('c',2),('d',4)])
>>> splitAtRLE 4 sampleRLE
([('a',1),('b',3)],[('c',2),('d',4)])
>>> splitAtRLE 5 sampleRLE
([('a',1),('b',3),('c',1)],[('c',1),('d',4)])
>>> splitAtRLE 6 sampleRLE
([('a',1),('b',3),('c',2)],[('d',4)])
-}
splitAtRLE :: Int -> RLE a -> (RLE a, RLE a)
splitAtRLE n = iter n []
    where
        iter m xs = \ case
            yk@(y,k):ys -> case compare m k of
                LT          -> (reverse ((y, m) : xs), (y, k - m) : ys)
                EQ          -> (reverse (yk : xs), ys)
                GT          -> iter (m - k) (yk : xs) ys
            _           -> (xs, [])

{- DECL MultiSet [] ["Data.Map qualified as M"] -}
{- | MultiSet
-}
type MultiSet a = M.Map a Int

{- DECL msEmpty [MultiSet] ["Data.Map qualified as M"] -}
{- | empty MultiSet
>>> msEmpty :: MultiSet Int
fromList []
-}
msEmpty :: MultiSet a
msEmpty = M.empty

{- DECL msNull [MultiSet] ["Data.Map qualified as M"] -}
{- | is True for empty MultiSet
>>> msNull msEmpty
True
>>> msNull (msSingleton 0)
False
-}
msNull :: MultiSet a -> Bool
msNull = M.null

{- DECL msSingleton [MultiSet] ["Data.Map qualified as M"] -}
{- | makes singleton MultiSet
>>> msSingleton (7 :: Int)
fromList [(7,1)]
-}
msSingleton :: a -> MultiSet a
msSingleton x = M.singleton x 1

{- DECL msSize [MultiSet] ["Data.Map qualified as M"] -}
{- | number of kinds of elements in MultiSet
-}
msSize :: Ord a => MultiSet a -> Int
msSize = M.size

{- DECL msInsert [MultiSet] ["Data.Map qualified as M"] -}
{- | insert a element into MultiSet
>>> msInsert (1 :: Int) msEmpty == msSingleton (1 :: Int)
True
>>> msInsert (1 :: Int) (msSingleton 0)
fromList [(0,1),(1,1)]
>>> msInsert (0 :: Int) (msSingleton 0)
fromList [(0,2)]
-}
msInsert :: Ord a => a -> MultiSet a -> MultiSet a
msInsert x = M.insertWith (+) x 1

{- DECL msDelete [MultiSet] ["Data.Map qualified as M"] -}
{- | delete a single element from MultiSet
>>> sample = msFromList [3,1,4,1,5,9,2,6,5 :: Int]
>>> sample
fromList [(1,2),(2,1),(3,1),(4,1),(5,2),(6,1),(9,1)]
>>> msDelete 1 sample
fromList [(1,1),(2,1),(3,1),(4,1),(5,2),(6,1),(9,1)]
>>> msDelete 2 sample
fromList [(1,2),(3,1),(4,1),(5,2),(6,1),(9,1)]
-}
msDelete :: Ord a => a -> MultiSet a -> MultiSet a
msDelete = M.update phi
    where
        phi = \ case
            1 -> Nothing
            c -> Just (pred c)

{- DECL msFromList [MultiSet, msEmpty, msInsert] ["Data.List","Data.Map qualified as M"] -}
{- | converts from List
>>> msFromList [3,1,4,1,5,9,2,6,5 :: Int]
fromList [(1,2),(2,1),(3,1),(4,1),(5,2),(6,1),(9,1)]
-}
msFromList :: Ord a => [a] -> MultiSet a
msFromList = foldl' (flip msInsert) msEmpty

{- DECL msToList [MultiSet] ["Data.List","Data.Map qualified as M"] -}
{- | converts to List
>>> sample = msFromList [3,1,4,1,5,9,2,6,5 :: Int]
>>> sample
fromList [(1,2),(2,1),(3,1),(4,1),(5,2),(6,1),(9,1)]
>>> msToList sample
[1,1,2,3,4,5,5,6,9]
-}
msToList :: Ord a => MultiSet a -> [a]
msToList = concatMap (uncurry (flip replicate)) . M.toList

{- DECL scanlarr [] ["Data.Array","Data.List"] -}
{- | transforms result list of scanl to array
>>> sampleList = take 5 [1 :: Int ..]
>>> sampleList
[1,2,3,4,5]
>>> scanlarr (+) 0 sampleList 5
array (0,5) [(0,0),(1,1),(2,3),(3,6),(4,10),(5,15)]
-}
scanlarr :: (b -> a -> b) -> b -> [a] -> Int -> Array Int b
scanlarr phi z xs n = listArray (0, n) (scanl' phi z xs)

{- DECL countSpan [] ["Data.List"] -}
{- | gives length of span and rest
>>> sampleList = "Relationship"
>>> sampleList
"Relationship"
>>> countSpan ('p' >) sampleList
(4,"tionship")
-}
countSpan :: (a -> Bool) -> [a] -> (Int, [a])
countSpan p = iter 0
    where
        iter !c = \ case
            x:xs | p x -> iter (succ c) xs
            xxs        -> (c, xxs)

{- DECL countif [] ["Data.Bool","Data.List"] -}
{- | count elements which satisfy predicate
>>> countif even [1..10]
5
-}
countif :: (a -> Bool) -> [a] -> Int
countif p = foldl' phi 0
    where
        phi a = \ case
            x | p x       -> succ a
              | otherwise -> a

{- DECL yesno [] ["Data.Bool","Data.String"] -}
{- | displays judge with "Yes" or "No"
>>> yesno (() /= ()) :: String
"No"
-}
yesno :: IsString a
      => Bool -> a
yesno = bool "No" "Yes"
