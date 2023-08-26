module Util where

import Data.Map qualified as M
import Static.Error (Error)

data Treither a b c
    = First a
    | Second b
    | Third c

intoTwo :: (a -> Either b c) -> [a] -> ([b], [c])
intoTwo _ [] = ([], [])
intoTwo f (x : xs) = f x <:> intoTwo f xs
  where
    e <:> (a, b) = case e of
        Left a' -> (a' : a, b)
        Right b' -> (a, b' : b)

duplicate :: (Eq a, Show a) => [a] -> [[a]]
duplicate [] = []
duplicate (x : xs) =
    case x `elem'` xs of
        ([], clean) -> duplicate clean
        (dups, clean) -> (x : dups) : duplicate clean
  where
    elem' _ [] = ([], [])
    elem' a (x : xs) =
        let (dups, clean) = a `elem'` xs
         in if a == x
                then (x : dups, clean)
                else (dups, x : clean)

insertLookup :: (Ord a) => a -> b -> M.Map a b -> (Maybe b, M.Map a b)
insertLookup = M.insertLookupWithKey (\_ a _ -> a)

union :: (Ord a) => M.Map a b -> M.Map a b -> Either [Error] (M.Map a b)
union a =
    M.foldlWithKey
        ( \a' bK' b' -> do
            aOk <- a'
            case insertLookup bK' b' aOk of
                (Nothing, m) -> Right m
                (Just _, _) -> Left []
        )
        (Right a)
