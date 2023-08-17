module Static.Duplicates where

import Parser.Function (Function (..))
import Parser.Type (TypeDef (..))
import Static.Error
import Util (duplicate)

duplicates :: [Function] -> [TypeDef] -> Either [Error] ()
duplicates fs ts =
  let (names, constructors) = typeDefNames ts
   in do
        duplicate' fs fName
        duplicate' names id
        duplicate' constructors id
  where
    duplicate' ds shower = case duplicate ds of
      [] -> Right ()
      dups ->
        Left $
          foldl
            ( \acc (o : os) ->
                Duplicate
                  (shower o)
                  (map shower os)
                  : acc
            )
            []
            dups

typeDefNames :: [TypeDef] -> ([String], [String])
typeDefNames =
  foldl
    ( \(ns, cs) t ->
        ( tName t : ns,
          map fst (tConstructors t) ++ cs
        )
    )
    ([], [])
