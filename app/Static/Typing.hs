{-# LANGUAGE LambdaCase #-}

module Static.Typing where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax (Lift)
import Parser.ExprStmt (Block, Expression (EBlock, EFunction, EIfElse, EInfix, ELiteral, EMatch, EType, EVar), Infix, InfixType (Boolean, Numeric), Statement (SAssignment, SExpr, SExprRet, SFor, SIf, SLet, SReturn, SWhile), infixType)
import Parser.Function (Function, Function' (Function, fBody, fReturn, fSignature))
import Parser.Literal (Literal (LArray, LChar, LNum, LString, LUnit))
import Parser.Pattern (Pattern (PCapture, PLiteral, PType, PVar, Pidc))
import Parser.Type (Type (TAny, TAnyTiedTo, TComplex, TSimple, TUnit, TVar), TypeDef)
import Parser.Util (IdenName)
import Static.Error (Error (AfterReturn, ArgumentLengths, FunctionAssignment, NonMutable, NotFound, NotFunction, TypeMismatch))
import Util (Treither (First, Second, Third), insertLookup, union')

data Statement'
    = SLet' Bool IdenName (Typed Expression')
    | SAssignment' IdenName (Typed Expression')
    | SIf' (Typed Expression') (Typed Expression')
    | SReturn' (Typed Expression')
    | SExpr' (Typed Expression')
    | SExprRet' (Typed Expression')
    | SFor' IdenName (Typed Expression') (Typed Expression')
    | SWhile' (Typed Expression') (Typed Expression')
    deriving (Show, Lift)

type Block' = [Statement']

data Expression'
    = ELiteral' (Typed (Literal (Typed Expression')))
    | EIfElse' (Typed Expression') (Typed Expression') (Typed Expression')
    | EMatch' (Typed Expression') [(Pattern, Maybe (Typed Expression'), Typed Expression')]
    | EFunction' IdenName [Typed Expression']
    | EVar' IdenName
    | EBlock' Block'
    | EInfix' (Typed Expression') Infix (Typed Expression')
    deriving (Show, Lift)

data Typed a = Typed a Type
    deriving (Show, Lift)

type FunctionTyped = Function' Block'

typeCheckFunction :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Function -> Either [Error] FunctionTyped
typeCheckFunction refs typeRefs func =
    let refs' = M.fromList (map (\(mut, name, t) -> (name, Third (mut, t))) $ fSignature func) <> refs
     in do
            (t, returns, block) <- typeCheckBlock refs' typeRefs $ fBody func
            let newF = func{fBody = block}
            let fRet = fromMaybe TUnit $ fReturn func
            case (fRet, t) of
                (ret, t') | ret == t' && all (ret ==) returns -> Right newF
                (ret, t') -> Left [TypeMismatch ret t']

typeCheckBlock :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Block -> Either [Error] (Type, [Type], Block')
typeCheckBlock _ _ [] = return (TUnit, [], [])
typeCheckBlock refs typeRefs (stmt : stmts) = case stmt of
    SLet mut tM name expr -> do
        (exprT, returns, expr') <- typeCheckExpr refs typeRefs expr
        case tM of
            Just t | t == exprT -> undefined
            Just t -> Left [TypeMismatch t exprT]
            Nothing -> do
                let (_, refs') = insertLookup name (Third (mut, exprT)) refs
                (t, returns', block) <- typeCheckBlock refs' typeRefs stmts
                return (t, returns ++ returns', SLet' mut name expr' : block)
    SAssignment "_" expr -> do
        (_, returns, expr') <- typeCheckExpr refs typeRefs expr
        (t, returns', block) <- typeCheckBlock refs typeRefs stmts
        return (t, returns ++ returns', SAssignment' "_" expr' : block)
    SAssignment name expr -> do
        t <- case name `M.lookup` refs of
            Just (First _) -> Left [FunctionAssignment name]
            Just (Second _) -> Left [FunctionAssignment name]
            Just (Third (False, _)) -> Left [NonMutable name]
            Just (Third (True, t)) -> Right t
            Nothing -> Left [NotFound name]
        (exprT, returns, expr') <- typeCheckExpr refs typeRefs expr
        if t == exprT
            then do
                (bT, returns', block) <- typeCheckBlock refs typeRefs stmts
                return (bT, returns ++ returns', SAssignment' name expr' : block)
            else Left [TypeMismatch t exprT]
    SIf cond body -> do
        (condT, returnsC, cond') <- typeCheckExpr refs typeRefs cond
        (bodyT, returnsB, body') <- typeCheckExpr refs typeRefs body
        unless (condT == TComplex "Bool" []) $ Left [TypeMismatch (TSimple "Bool") condT]
        unless (bodyT == TUnit) $ Left [TypeMismatch TUnit bodyT]
        (bT, returns, block) <- typeCheckBlock refs typeRefs stmts
        return (bT, returnsC ++ returnsB ++ returns, SIf' cond' body' : block)
    SWhile cond body -> do
        (condT, returnsC, cond') <- typeCheckExpr refs typeRefs cond
        (bodyT, returnsB, body') <- typeCheckExpr refs typeRefs body
        unless (condT == TComplex "Bool" []) $ Left [TypeMismatch condT (TSimple "Bool")]
        unless (bodyT == TUnit) $ Left [TypeMismatch bodyT TUnit]
        (bT, returns, block) <- typeCheckBlock refs typeRefs stmts
        return (bT, returnsC ++ returnsB ++ returns, SWhile' cond' body' : block)
    SFor name iter body -> do
        (iterT, returnsI, iter') <- typeCheckExpr refs typeRefs iter
        nameT <- case iterT of
            TComplex "Vector" [elemT] -> return elemT
            t -> Left [TypeMismatch (TComplex "Vector" [TAny]) t]
        let (_, refs') = insertLookup name (Third (False, nameT)) refs
        (bodyT, returnsB, body') <- typeCheckExpr refs' typeRefs body
        unless (bodyT == TUnit) $ Left [TypeMismatch TUnit bodyT]
        (bT, returns, block) <- typeCheckBlock refs typeRefs stmts
        return (bT, returnsI ++ returnsB ++ returns, SFor' name iter' body' : block)
    SReturn expr -> do
        (exprT, returns, expr') <- typeCheckExpr refs typeRefs expr
        unless (null stmts) $ Left [AfterReturn stmts]
        return (TUnit, exprT : returns, [SReturn' expr'])
    SExpr expr -> do
        (exprT, returnsE, expr') <- typeCheckExpr refs typeRefs expr
        unless (exprT == TUnit) $ Left [TypeMismatch TUnit exprT]
        (bT, returns, block) <- typeCheckBlock refs typeRefs stmts
        return (bT, returnsE ++ returns, SExpr' expr' : block)
    SExprRet expr -> do
        (exprT, returns, expr') <- typeCheckExpr refs typeRefs expr
        unless (null stmts) $ Left [AfterReturn stmts]
        return (exprT, returns, [SExprRet' expr'])

typeCheckExpr :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Expression -> Either [Error] (Type, [Type], Typed Expression')
typeCheckExpr refs typeRefs expression =
    case expression of
        ELiteral (LNum s n) -> return (TSimple "Int", [], Typed (ELiteral' (Typed (LNum s n) (TSimple "Int"))) (TSimple "Int"))
        ELiteral (LString s) -> return (TSimple "String", [], Typed (ELiteral' (Typed (LString s) (TSimple "String"))) (TSimple "String"))
        ELiteral (LChar c) -> return (TSimple "Char", [], Typed (ELiteral' (Typed (LChar c) (TSimple "Char"))) (TSimple "Char"))
        ELiteral LUnit -> return (TUnit, [], Typed (ELiteral' (Typed LUnit TUnit)) TUnit)
        ELiteral (LArray []) -> return (TComplex "Vector" [TAny], [], Typed (ELiteral' (Typed (LArray []) (TComplex "Vector" [TAny]))) (TComplex "Vector" [TAny]))
        ELiteral (LArray exprs) -> do
            ts <- mapM (typeCheckExpr refs typeRefs) exprs
            let (t, returns, _) = head ts
            if all ((t ==) . (\(r, _, _) -> r)) ts
                then
                    return
                        ( TComplex "Vector" [t]
                        , foldl (\rs (_, r, _) -> r ++ rs) returns $ tail ts
                        , Typed
                            ( ELiteral'
                                ( Typed
                                    (LArray $ map (\(_, _, r) -> r) ts)
                                    (TComplex "Vector" [t])
                                )
                            )
                            $ TComplex "Vector" [t]
                        )
                else Left []
        EIfElse cond true false -> do
            (condT, returnsC, cond') <- typeCheckExpr refs typeRefs cond
            (trueT, returnsT, true') <- typeCheckExpr refs typeRefs true
            (falseT, returnsF, false') <- typeCheckExpr refs typeRefs false
            if condT == TComplex "Bool" []
                then
                    if trueT == falseT
                        then return (trueT, returnsC ++ returnsT ++ returnsF, Typed (EIfElse' cond' true' false') trueT)
                        else Left [TypeMismatch trueT falseT]
                else Left [TypeMismatch (TSimple "Bool") condT]
        EMatch expr branches -> do
            (exprT, returnsM, mExpr) <- typeCheckExpr refs typeRefs expr
            let tRefs =
                    M.map
                        ( \case
                            Second x -> x
                            _ -> undefined
                        )
                        $ M.filter
                            ( \case
                                Second _ -> True
                                _ -> False
                            )
                            refs
            branches' <-
                mapM
                    ( \(pat, cond, body) -> do
                        (patT, refs') <- typePattern pat tRefs
                        refsU <- refs `union'` M.map (\v -> Third (False, v)) refs'
                        cond' <- case typeCheckExpr refsU typeRefs <$> cond of
                            Just r -> fmap Just $ do
                                (condT, returnsC, cond') <- r
                                if condT == TSimple "Bool"
                                    then return (condT, returnsC, cond')
                                    else Left [TypeMismatch (TSimple "Bool") condT]
                            Nothing -> Right Nothing
                        bodyT <- typeCheckExpr refsU typeRefs body
                        return (patT, cond', bodyT)
                    )
                    branches
            foldl
                ( \acc (patT, _, _) -> do
                    acc >> same' exprT patT
                )
                (Right ())
                branches'
            eT <- case map (\(_, _, b) -> b) branches' of
                [] -> undefined
                [(b, _, _)] -> return b
                ((b, _, _) : bs) ->
                    foldl
                        ( \acc b'T -> do
                            aT <- acc
                            same' aT b'T
                            return aT
                        )
                        (Right b)
                        $ map (\(r, _, _) -> r) bs
            let returns =
                    concatMap
                        ( \(_, c, (_, returnsE, _)) ->
                            ( case c of
                                Just (_, returnsC, _) -> returnsC
                                _ -> []
                            )
                                ++ returnsE
                        )
                        branches'
            return
                ( eT
                , returnsM ++ returns
                , Typed
                    ( EMatch' mExpr
                        $ map
                            ( \(pat, (_, cond, (_, _, expr'))) ->
                                ( pat
                                , fmap (\(_, _, r) -> r) cond
                                , expr'
                                )
                            )
                        $ zip
                            (map (\(pat, _, _) -> pat) branches)
                            branches'
                    )
                    eT
                )
        EFunction name args -> do
            (sig, ret) <- case name `M.lookup` refs of
                Just (First Function{fSignature, fReturn}) -> Right (map (\(_, _, t) -> t) fSignature, fromMaybe TUnit fReturn)
                Just (Second t) -> Right t
                Just (Third _) -> Left [NotFunction name]
                Nothing -> Left [NotFound name]
            let lenFSigTs = length sig
            let lenArgs = length args
            if lenFSigTs == lenArgs
                then do
                    (retT, returns, args') <- walkSignature args sig ret
                    return (retT, returns, Typed (EFunction' name args') retT)
                else Left [ArgumentLengths lenFSigTs lenArgs]
        EVar name ->
            case name `M.lookup` refs of
                Just (First _) -> error "No support for function types rn"
                Just (Second ([], ret)) -> return $ toExpType ret (EVar' name)
                Just (Second (_, _)) -> error "No support for function types rn"
                Just (Third (_, t)) -> return $ toExpType t (EVar' name)
                Nothing -> Left [NotFound name]
        EBlock block -> do
            (bT, returns, block') <- typeCheckBlock refs typeRefs block
            return (bT, returns, Typed (EBlock' block') bT)
        EType expr t -> do
            (t', returns, expr') <- typeCheckExpr refs typeRefs expr
            if t == t'
                then return (t', returns, expr')
                else Left [TypeMismatch t t']
        EInfix left infx right -> do
            let (lhs, rhs, ret) = infixType infx
            (l, returnsL, left') <- typeCheckExpr refs typeRefs left
            (r, returnsR, right') <- typeCheckExpr refs typeRefs right
            let checkT iT t =
                    let tmp = infixTType iT
                     in if tmp == t
                            then Right ()
                            else Left [TypeMismatch tmp t]
            _ <- checkT lhs l
            _ <- checkT rhs r
            let iT = infixTType ret
            return (iT, returnsL ++ returnsR, Typed (EInfix' left' infx right') iT)
  where
    walkSignature :: [Expression] -> [Type] -> Type -> Either [Error] (Type, [Type], [Typed Expression'])
    walkSignature (arg : args) (sig : sigs) ret = do
        (argT, returns, arg') <- typeCheckExpr refs typeRefs arg
        case sig of
            _ | argT == sig -> do
                (wT, returns', args') <- walkSignature args sigs ret
                return (wT, returns ++ returns', arg' : args')
            _ -> case same sig argT of
                Just replace ->
                    walkSignature
                        args
                        ( map
                            ( \sig' ->
                                foldl
                                    (\acc (name, t) -> changeVars name t acc)
                                    sig'
                                    replace
                            )
                            sigs
                        )
                        $ foldl
                            ( \acc
                               (name, t) -> changeVars name t acc
                            )
                            ret
                            replace
                Nothing -> Left [TypeMismatch sig argT]
    walkSignature _ _ ret = return (ret, [], [])

    same :: Type -> Type -> Maybe [(String, Type)]
    same (TVar name) t = Just [(name, t)]
    same t (TVar name) = Just [(name, t)]
    same (TSimple t) (TSimple t') =
        if t == t'
            then Just []
            else Nothing
    same (TComplex name ts) (TComplex name' ts') =
        if name == name'
            then foldl (\acc (t, t') -> acc <> same t t') Nothing $ zip ts ts'
            else Nothing
    same TUnit TUnit = Just []
    same _ _ = Nothing

    same' TAny _ = Right ()
    same' _ TAny = Right ()
    same' t@(TSimple n) t'@(TSimple n') =
        if n == n'
            then Right ()
            else Left [TypeMismatch t t']
    same' t@(TComplex n a) t'@(TComplex n' a') =
        if n == n'
            then do
                zipped <- do
                    let lenA = length a
                    let lenA' = length a'
                    if lenA == lenA'
                        then Right $ zip a a'
                        else Left [ArgumentLengths lenA lenA']
                foldl
                    (\acc (a, a') -> acc >> same' a a')
                    (Right ())
                    zipped
            else Left [TypeMismatch t t']
    same' (TVar _) (TVar _) = Right ()
    same' (TVar _) _ = Right ()
    same' _ (TVar _) = Right ()
    same' TUnit TUnit = Right ()
    same' t t' = Left [TypeMismatch t t']

    infixTType Numeric = TSimple "Int"
    infixTType Boolean = TSimple "Bool"

    toExpType :: Type -> a -> (Type, [Type], Typed a)
    toExpType t expr = (t, [], Typed expr t)

changeVars :: String -> Type -> Type -> Type
changeVars var to on = case on of
    TVar name | var == name -> to
    TComplex name ts -> TComplex name $ map (changeVars var to) ts
    other -> other

typePattern :: Pattern -> M.Map String ([Type], Type) -> Either [Error] (Type, M.Map String Type)
typePattern pattern typeRefs =
    case pattern of
        PCapture name pat -> do
            (t, refs) <- typePattern pat typeRefs
            refs' <- refs `union'` M.singleton name t
            return (t, refs')
        PVar name -> return (TAnyTiedTo name, M.singleton name TAny)
        PType name pats -> do
            (sig, ret) <- case name `M.lookup` typeRefs of
                Just r -> Right r
                Nothing -> Left [NotFound name]
            zipped <- do
                let lenSig = length sig
                let lenPats = length pats
                if lenSig == lenPats
                    then Right $ zip sig pats
                    else Left [ArgumentLengths lenSig lenPats]
            (tRefs, pRefs) <-
                foldl
                    ( \acc (s, p) -> do
                        (tRefs, pRefs) <- acc
                        (pT, pRefs') <- typePattern p typeRefs
                        let pT' = M.foldlWithKey (\acc' k v -> changeVars k v acc') pT tRefs
                        pRefsU <- pRefs `union'` pRefs'
                        (pRefsU', tRefs') <- same s pT' pRefsU
                        tRefsU <- tRefs `union'` tRefs'
                        return (tRefsU, pRefsU')
                    )
                    (Right (M.empty, M.empty))
                    zipped
            return (M.foldlWithKey (\acc k tRef -> changeVars k tRef acc) ret tRefs, pRefs)
        PLiteral (LArray []) -> return (TComplex "Vector" [TAny], M.empty)
        PLiteral (LArray es) -> do
            (refs, ts') <-
                foldl
                    ( \acc e -> do
                        (refs, ts) <- acc
                        (t, refs') <- typePattern e typeRefs
                        refsU <- refs `union'` refs'
                        return (refsU, t : ts)
                    )
                    (Right (M.empty, []))
                    es
            let (t, ts) = (head ts', tail ts')
            (ret, refs') <-
                foldl
                    ( \acc t' -> do
                        (lastT, refs') <- acc
                        same' t' lastT refs'
                    )
                    (Right (t, refs))
                    ts
            return (TComplex "Vector" [ret], refs')
        PLiteral (LNum _ _) -> return (TSimple "Int", M.empty)
        PLiteral (LChar _) -> return (TSimple "Char", M.empty)
        PLiteral (LString _) -> return (TSimple "String", M.empty)
        PLiteral LUnit -> return (TUnit, M.empty)
        Pidc -> return (TAny, M.empty)
  where
    same :: Type -> Type -> M.Map String Type -> Either [Error] (M.Map String Type, M.Map String Type)
    same (TAnyTiedTo n) t refs =
        case n `M.lookup` refs of
            Just TAny -> let refs' = M.insert n t refs in return (refs', M.empty)
            Just _ -> error "this also shouldn't be possible"
            Nothing -> Left [NotFound n]
    same t (TAnyTiedTo n) refs =
        case n `M.lookup` refs of
            Just TAny -> let refs' = M.insert n t refs in return (refs', M.empty)
            Just _ -> error "this also shouldn't be possible"
            Nothing -> Left [NotFound n]
    same (TVar n) t refs = return (refs, M.singleton n t)
    same t@(TSimple n) t'@(TSimple n') refs =
        if n == n'
            then return (refs, M.empty)
            else Left [TypeMismatch t t']
    same t@(TComplex n a) t'@(TComplex n' a') refs =
        if n == n'
            then do
                zipped <- do
                    let lenA = length a
                    let lenA' = length a'
                    if lenA == lenA'
                        then Right $ zip a a'
                        else Left [ArgumentLengths lenA lenA']
                foldl
                    ( \acc (a, a') -> do
                        (pRefs, refs) <- acc
                        (pRefs', refs') <- same (changeVars' a refs) (changeVars' a' refs) pRefs
                        refsU <- refs `union'` refs'
                        return (pRefs', refsU)
                    )
                    (Right (refs, M.empty))
                    zipped
            else Left [TypeMismatch t t']
    same TAny _ refs = return (refs, M.empty)
    same _ TAny refs = return (refs, M.empty)
    same TUnit TUnit refs = return (refs, M.empty)
    same t t' _ = Left [TypeMismatch t t']

    changeVars' = M.foldlWithKey (\acc k v -> changeVars k v acc)

    same' (TAnyTiedTo name) t refs =
        case name `M.lookup` refs of
            Just TAny -> let refs' = M.insert name t refs in return (t, refs')
            Just _ -> error "this also shouldn't be possible"
            Nothing -> Left [NotFound name]
    same' t@(TSimple n) t'@(TSimple n') refs =
        if n == n'
            then return (t, refs)
            else Left [TypeMismatch t t']
    same' t@(TComplex n a) t'@(TComplex n' a') refs =
        if n == n'
            then do
                zipped <- do
                    let lenA = length a
                    let lenA' = length a'
                    if lenA == lenA'
                        then return $ zip a a'
                        else Left [ArgumentLengths lenA lenA']
                refs' <-
                    foldl
                        ( \acc (a, a') -> do
                            refsA <- acc
                            (_, refs') <- same' a a' refsA
                            refs `union'` refs'
                        )
                        (Right refs)
                        zipped
                return (t, refs')
            else Left [TypeMismatch t t']
    same' TUnit TUnit refs = return (TUnit, refs)
    same' TAny t refs = return (t, refs)
    same' _ TAny _ = error "laso don't know if possible"
    same' t t' _ = Left [TypeMismatch t t']
