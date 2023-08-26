{-# LANGUAGE LambdaCase #-}

module Static.Typing where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShow, traceShowId)
import Parser.ExprStmt (Block, Expression (EBlock, EFunction, EIfElse, EInfix, ELiteral, EMatch, EType, EVar), InfixType (Boolean, Numeric), Statement (SAssignment, SExpr, SExprRet, SIf, SLet, SReturn), infixType)
import Parser.Function (Function (Function, fBody, fReturn, fSignature))
import Parser.Literal (Literal (LArray, LChar, LNum, LString, LUnit))
import Parser.Pattern (Pattern (PCapture, PLiteral, PType, PVar, Pidc))
import Parser.Type (Type (TAny, TAnyTiedTo, TComplex, TReturn, TSimple, TUnit, TVar), TypeDef)
import Static.Error (Error (AfterReturn, ArgumentLengths, FunctionAssignment, NonMutable, NotFound, NotFunction, TypeMismatch))
import Util (Treither (First, Second, Third), insertLookup, union)

typeCheckFunction :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Function -> Either [Error] ()
typeCheckFunction refs typeRefs func =
    let refs' = M.fromList (map (\(mut, name, t) -> (name, Third (mut, t))) $ fSignature func) <> refs
     in do
            t <- typeCheckBlock refs' typeRefs $ fBody func
            case (fReturn func, t) of
                (Just ret, TReturn t') | ret == t' -> Right ()
                (Nothing, TReturn t') | t' == TUnit -> Right ()
                (Just ret, t') | ret == t' -> Right ()
                (Nothing, t') | t' == TUnit -> Right ()
                (Just ret, t') -> Left [TypeMismatch ret t']
                (Nothing, t') -> Left [TypeMismatch TUnit t']

typeCheckBlock :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Block -> Either [Error] Type
typeCheckBlock _ _ [] = Right TUnit
typeCheckBlock refs typeRefs (stmt : stmts) = case stmt of
    SLet mut tM name expr -> do
        t' <- typeCheckExpr refs typeRefs expr
        case t' of
            TReturn _ -> Right t'
            _ ->
                case tM of
                    Just t | t == t' -> undefined
                    Just t -> Left [TypeMismatch t t']
                    Nothing ->
                        let (_, refs') = insertLookup name (Third (mut, t')) refs
                         in typeCheckBlock refs' typeRefs stmts
    SAssignment "_" expr -> do
        _ <- typeCheckExpr refs typeRefs expr
        typeCheckBlock refs typeRefs stmts
    SAssignment name expr -> do
        t <- case name `M.lookup` refs of
            Just (First _) -> Left [FunctionAssignment name]
            Just (Second _) -> Left [FunctionAssignment name]
            Just (Third (False, _)) -> Left [NonMutable name]
            Just (Third (True, t')) -> Right t'
            Nothing -> Left [NotFound name]
        t' <- typeCheckExpr refs typeRefs expr
        if t == t'
            then typeCheckBlock refs typeRefs stmts
            else Left [TypeMismatch t t']
    SIf cond body -> do
        tCond <- typeCheckExpr refs typeRefs cond
        tBody <- typeCheckExpr refs typeRefs body
        if tCond == TSimple "Bool"
            then
                if tBody == TUnit
                    then typeCheckBlock refs typeRefs stmts
                    else Left [TypeMismatch TUnit tBody]
            else Left [TypeMismatch (TSimple "Bool") tCond]
    SReturn expr -> do
        t <- typeCheckExpr refs typeRefs expr
        if null stmts
            then Right $ TReturn t
            else Left [AfterReturn stmts]
    SExpr expr -> do
        t <- typeCheckExpr refs typeRefs expr
        case t of
            TUnit -> typeCheckBlock refs typeRefs stmts
            TReturn t' -> return t'
            _ -> Left [TypeMismatch TUnit t]
    SExprRet expr -> do
        t <- typeCheckExpr refs typeRefs expr
        if null stmts
            then Right t
            else Left [AfterReturn stmts]
    _ -> typeCheckBlock refs typeRefs stmts

typeCheckExpr :: M.Map String (Treither Function ([Type], Type) (Bool, Type)) -> M.Map String TypeDef -> Expression -> Either [Error] Type
typeCheckExpr refs typeRefs expression =
    case expression of
        ELiteral (LNum _ _) -> Right $ TSimple "Int"
        ELiteral (LString _) -> Right $ TSimple "String"
        ELiteral (LChar _) -> Right $ TSimple "Char"
        ELiteral LUnit -> Right TUnit
        ELiteral (LArray []) -> Right TAny
        ELiteral (LArray exprs) -> do
            ts <- mapM (typeCheckExpr refs typeRefs) exprs
            let t = head ts
            if all (t ==) ts
                then Right t
                else Left []
        EIfElse cond true false -> do
            condT <- typeCheckExpr refs typeRefs cond
            trueT <- typeCheckExpr refs typeRefs true
            falseT <- typeCheckExpr refs typeRefs false
            if condT == TSimple "Bool"
                then
                    if trueT == falseT
                        then Right trueT
                        else Left [TypeMismatch trueT falseT]
                else Left [TypeMismatch (TSimple "Bool") condT]
        EMatch expr branches -> do
            exprT <- typeCheckExpr refs typeRefs expr
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
                        refsU <- refs `union` M.map (\v -> Third (False, v)) refs'
                        condT <- case typeCheckExpr refsU typeRefs <$> cond of
                            Just r -> fmap Just $ do
                                t <- r
                                if t == TSimple "Bool"
                                    then return t
                                    else Left [TypeMismatch (TSimple "Bool") t]
                            Nothing -> Right Nothing
                        bodyT <- typeCheckExpr refsU typeRefs body
                        return (patT, condT, bodyT)
                    )
                    branches
            foldl
                ( \acc (patT, _, _) -> do
                    acc >> same' exprT patT
                )
                (Right ())
                branches'
            case map (\(_, _, body') -> body') branches' of
                [] -> undefined
                [b] -> return b
                (b : bs) ->
                    foldl
                        ( \acc b' -> do
                            a <- acc
                            same' a b'
                            return a
                        )
                        (Right b)
                        bs
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
                    ret' <- walkSignature args sig ret
                    Right ret'
                else Left [ArgumentLengths lenFSigTs lenArgs]
        EVar name ->
            case name `M.lookup` refs of
                Just (First _) -> error "No support for function types rn"
                Just (Second ([], ret)) -> Right ret
                Just (Second (_, _)) -> error "No support for function types rn"
                Just (Third (_, t)) -> Right t
                Nothing -> Left [NotFound name]
        EBlock block -> typeCheckBlock refs typeRefs block
        EType expr t -> do
            t' <- typeCheckExpr refs typeRefs expr
            if t == t'
                then Right t
                else Left [TypeMismatch t t']
        EInfix left infx right -> do
            let (lhs, rhs, ret) = infixType infx
            l <- typeCheckExpr refs typeRefs left
            r <- typeCheckExpr refs typeRefs right
            let checkT iT t =
                    let tmp = infixTType iT
                     in if tmp == t
                            then Right ()
                            else Left [TypeMismatch tmp t]
            _ <- checkT lhs l
            _ <- checkT rhs r
            return $ infixTType ret
  where
    walkSignature :: [Expression] -> [Type] -> Type -> Either [Error] Type
    walkSignature (arg : args) (sig : sigs) ret = do
        argT <- typeCheckExpr refs typeRefs arg
        case sig of
            _ | argT == sig -> walkSignature args sigs ret
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
    walkSignature _ _ ret = Right ret

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
    same (TReturn t) (TReturn t') = same t t'
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
    same' (TVar n) (TVar n') = Right ()
    same' (TVar _) _ = Right ()
    same' _ (TVar _) = Right ()
    same' TUnit TUnit = Right ()
    same' (TReturn t) t' = same' t t'
    same' t (TReturn t') = same' t t'
    same' t t' = Left [TypeMismatch t t']

    infixTType Numeric = TSimple "Int"
    infixTType Boolean = TSimple "Bool"

changeVars :: String -> Type -> Type -> Type
changeVars var to on = case on of
    TVar name | var == name -> to
    TComplex name ts -> TComplex name $ map (changeVars var to) ts
    TReturn t -> TReturn $ changeVars var to t
    other -> other

typePattern :: Pattern -> M.Map String ([Type], Type) -> Either [Error] (Type, M.Map String Type)
typePattern pattern typeRefs =
    case pattern of
        PCapture name pat -> do
            (t, refs) <- typePattern pat typeRefs
            refs' <- refs `union` M.singleton name t
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
                        pRefsU <- pRefs `union` pRefs'
                        (pRefsU', tRefs') <- same s pT' pRefsU
                        tRefsU <- tRefs `union` tRefs'
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
                        refsU <- refs `union` refs'
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
                let w =
                        foldl
                            ( \acc (a, a') -> do
                                (pRefs, refs) <- acc
                                (pRefs', refs') <- same (changeVars' a refs) (changeVars' a' refs) pRefs
                                refsU <- refs `union` refs'
                                pRefsU <- pRefs `union` pRefs'
                                return (pRefsU, refsU)
                            )
                            (Right (refs, M.empty))
                            zipped
                undefined
            else Left [TypeMismatch t t']
    same TAny _ refs = return (refs, M.empty)
    same _ TAny refs = return (refs, M.empty)
    same _ TAny refs = return (refs, M.empty)
    same (TReturn _) (TReturn _) _ = error "this should be possible"
    same TUnit TUnit refs = return (refs, M.empty)
    same TAny TAny _ = error "don't know what to do with this for now"
    same (TAnyTiedTo _n) (TAnyTiedTo _n') _ = error "don't know what to do with this for now"
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
                refs <-
                    foldl
                        ( \acc (a, a') -> do
                            refsA <- acc
                            (_, refs') <- same' a a' refsA
                            refs `union` refs'
                        )
                        (Right refs)
                        zipped
                return (t, refs)
            else Left [TypeMismatch t t']
    same' TUnit TUnit refs = return (TUnit, refs)
    same' TAny t refs = return (t, refs)
    same' _ TAny _ = error "laso don't know if possible"
    same' t t' _ = Left [TypeMismatch t t']
