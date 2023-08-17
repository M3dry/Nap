module Util where

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
