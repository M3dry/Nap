module ToC.DependencyMaker where

import Control.Monad.Except (Except, MonadError (throwError))
import Parser.Type (Type (TComplex), TypeDef (TypeDef))
import ToC.CDSL (CError, CFunctionDef, CStructDef, CType)
import ToC.TypeDef (typeDefToC, typeToC)

-- typeDefRelations :: [(TypeDef, [(String, CType)])] -> Except [CError] [([(CStructDef, CFunctionDef)], CStructDef, [CFunctionDef], CFunctionDef)]
-- typeDefRelations tds = do
--   let tds' = map (\(t@(TypeDef n _ _), refs) -> (n, forTypeDef t, refs)) tds
--   let tfs =
--         map
--           ( ( \td@(TypeDef n gens _) ->
--                 ( n,
--                   \refs ->
--                     typeDefToC td $ case varLTo gens refs of
--                       Just r -> r
--                       _ -> undefined
--                 )
--             )
--               . fst
--           )
--           tds
--   undefined

create :: [(TypeDef, [(String, CType)])] -> Except [CError] [([(CStructDef, CFunctionDef)], CStructDef, [CFunctionDef], CFunctionDef)]
create [] = return []
create tds = create' tds []
  where
    create' [] _ = return []
    create' (tp@(t@(TypeDef tN tv _), tVars) : tds) created = do
        tVars' <- case varLTo tv tVars of
            Just r -> return r
            _ -> throwError []
        t' <- typeDef t tVars
        let tds' = map (\(t'@(TypeDef n _ _), _) -> (n, t')) $ tp : tds
        r <- typeDefToC t tVars'
        (created', r') <-
            foldl
                ( \acc (n, cts) -> do
                    (created', acc') <- acc
                    case n `lookup` tds' of
                            Just l | (n, cts) `notElem` created' -> do
                                tmp <- typeDefToC l cts
                                return ((n, cts) : created', tmp : acc')
                            Just _ -> return (created', acc')
                            _ -> throwError []
                )
                (return ((tN, tVars') : created, []))
                t'
        rs <- create' tds created'
        return $ r : r' ++ rs

typeDef :: TypeDef -> [(String, CType)] -> Except [CError] [(String, [CType])]
typeDef td tVars =
    mapM
        ( \(f, s) -> do
            s' <- mapM (`typeToC` tVars) s
            return (f, s')
        )
        $ typeDefTs td
  where
    typeDefTs :: TypeDef -> [(String, [Type])]
    typeDefTs (TypeDef _ _ cons) =
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
