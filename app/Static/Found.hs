module Static.Found where

import Parser.ExprStmt
  ( Block,
    Expression
      ( EBlock,
        EFunction,
        EIfElse,
        EInfix,
        ELiteral,
        EMatch,
        EType,
        EVar
      ),
    Statement
      ( SAssignment,
        SExpr,
        SFor,
        SIf,
        SLet,
        SReturn,
        SWhile
      ),
  )
import Parser.Function
  ( Function
      ( Function,
        fBody,
        fSignature
      ),
  )
import Parser.Literal (Literal (LArray))
import Parser.Pattern
  ( Pattern
      ( PCapture,
        PLiteral,
        PType,
        PVar
      ),
  )
import Parser.Type
  ( Type
      ( TComplex,
        TSimple,
        TVar
      ),
    TypeDef (tConstructors, tVars),
  )
import Static.Error (Error (NotFound))

checkFunction :: [String] -> [String] -> Function -> Either [Error] ()
checkFunction refs typeRefs (Function {fSignature, fBody}) =
  do
    sigRefs <- checkSignature typeRefs fSignature
    checkBlock (refs <> sigRefs) fBody
  where
    checkSignature typeRefs' signature =
      let (names, ts) =
            foldl
              ( \(names', ts') (_, name, t) ->
                  (name : names', t : ts')
              )
              ([], [])
              signature
       in foldl
            ( \acc t ->
                acc
                  >> checkType
                    (const $ Right ())
                    typeRefs'
                    t
            )
            (Right ())
            ts
            >> Right names

checkType :: (String -> Either [Error] ()) -> [String] -> Type -> Either [Error] ()
checkType f typeRefs t = case t of
  TSimple name -> exists typeRefs name
  TComplex name ts ->
    exists typeRefs name
      >> foldl
        (\acc t' -> acc >> checkType f typeRefs t')
        (Right ())
        ts
  TVar name -> f name

checkBlock :: [String] -> Block -> Either [Error] ()
checkBlock _ [] = Right ()
checkBlock refs (stmt : stmts) =
  case stmt of
    SLet _ _ pat _ -> checkBlock (getVarsPattern pat <> refs) stmts
    SAssignment var expr -> exists refs var >> exprsStep refs [expr]
    SIf cond true -> exprsStep refs [cond, true]
    SReturn expr -> exprsStep refs [expr]
    SExpr expr -> exprsStep refs [expr]
    SFor pat iter body ->
      let refs' = getVarsPattern pat <> refs
       in exprsStep refs' [iter, body]
    SWhile cond body -> exprsStep refs [cond, body]
  where
    exprsStep _ [] = Right ()
    exprsStep refs' (expr : exprs) =
      case expr of
        EFunction name args -> exists refs' name >> exprsStep refs' args
        EVar name -> exists refs' name
        EType expr' _ -> exprsStep refs' [expr']
        EInfix expr1 _ expr2 -> exprsStep refs' [expr1, expr2]
        EIfElse cond true false -> exprsStep refs' [cond, true, false]
        EMatch expr' branches ->
          exprsStep refs' [expr']
            >> foldl
              ( \acc (pat, cond, branch) ->
                  let nRefs = getVarsPattern pat <> refs'
                      exprs' = maybe [branch] ((<> [branch]) . pure) cond
                   in acc >> exprsStep nRefs exprs'
              )
              (Right ())
              branches
        EBlock block -> checkBlock refs' block
        ELiteral (LArray exprs') -> exprsStep refs' exprs'
        ELiteral _ -> exprsStep refs' exprs

exists :: [String] -> String -> Either [Error] ()
exists refs' name
  | name `elem` refs' = Right ()
  | otherwise = Left [NotFound name]

getVarsPattern :: Pattern -> [String]
getVarsPattern pattern =
  case pattern of
    PCapture var pat -> var : getVarsPattern pat
    PVar var -> [var]
    PType _ pats -> concatMap getVarsPattern pats
    PLiteral (LArray pats) -> concatMap getVarsPattern pats
    _ -> []

checkTypeDef :: [String] -> TypeDef -> Either [Error] ()
checkTypeDef refs t =
  let refs' = tVars t <> refs
   in foldl
        (\acc t' -> acc >> checkType (exists refs') refs' t')
        (Right ())
        $ concatMap snd
        $ tConstructors t
