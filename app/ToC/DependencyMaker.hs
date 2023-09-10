module ToC.DependencyMaker where

import Control.Monad.Except (Except)
import Parser.Type (Type (TComplex), TypeDef (TypeDef))
import ToC.CDSL (CError, CFunctionDef, CStructDef, CType)
import ToC.TypeDef (typeDefToC)

typeDefRelations :: [(TypeDef, [(String, CType)])] -> Except [CError] [([(CStructDef, CFunctionDef)], CStructDef, [CFunctionDef], CFunctionDef)]
typeDefRelations tds = do
  let tds' = map (\(t@(TypeDef n _ _), refs) -> (n, forTypeDef t, refs)) tds
  let tfs =
        map
          ( ( \td@(TypeDef n gens _) ->
                ( n,
                  \refs ->
                    typeDefToC td $ case varLTo gens refs of
                      Just r -> r
                      _ -> undefined
                )
            )
              . fst
          )
          tds
  undefined

forTypeDef :: TypeDef -> [(String, [Type])]
forTypeDef (TypeDef _ _ cons) =
  concatMap
    (\(_, ts) -> foldl (\acc t -> typeToDeps t ++ acc) [] ts)
    cons

typeToDeps :: Type -> [(String, [Type])]
typeToDeps (TComplex n ts) = (n, ts) : concatMap typeToDeps ts
typeToDeps _ = []

varLTo :: [String] -> [(String, CType)] -> Maybe [CType]
varLTo [] _ = return []
varLTo (a : as) bs = do
  rs <- varLTo as bs
  r <- a `lookup` bs
  return $ r : rs
