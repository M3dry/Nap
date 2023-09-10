module ToC.TypeDef where

import Control.Monad.Except (Except, MonadError (throwError), zipWithM)
import Parser.Type (
    Type (
        TComplex,
        TSimple,
        TUnit,
        TVar
    ),
    TypeDef (
        TypeDef,
        tConstructors,
        tName,
        tVars
    ),
 )
import ToC.CDSL (
    CError (
        TypeVarNotFound,
        VarLengthsMismatch
    ),
    CExpression (CEDeref, CEFunction, CEInfix, CENot, CENull, CENum, CERef, CESizeof, CEString, CEVar),
    CFunctionDec (CFunctionDec),
    CFunctionDef (CFunctionDef),
    CIden (CIden, CSCArr, CSCDot),
    CInfix (CIEq),
    CStatement (CSAssign, CSAssignDeref, CSExpr, CSIf, CSIfElse, CSReturn, CSVarDec),
    CStructDef (CStructDef),
    CType (CTName, CTPointer, CTStruct),
    CTypeDefT (CTDTNormal, CTDTUnion),
    cTypeName,
 )

typeDefToC :: TypeDef -> [CType] -> Except [CError] ([(CStructDef, CFunctionDef)], CStructDef, [CFunctionDef], CFunctionDef)
typeDefToC TypeDef{tName, tVars, tConstructors} cVars = do
    varLookup <- do
        let tVLen = length tVars
        let cVLen = length cVars
        if tVLen == cVLen
            then return $ zip tVars cVars
            else throwError [VarLengthsMismatch tVLen cVLen]
    let structName = "_" ++ tName ++ foldl (\str cVar -> str ++ "__" ++ cTypeName cVar) "" cVars
    constructors <- zipWithM (\t i -> constructor i structName varLookup t) tConstructors [1 ..]
    let helperStructs =
            foldl
                ( \acc m -> case m of
                    Just (d, f) -> (d, f) : acc
                    Nothing -> acc
                )
                []
                $ map (\(a, _, _) -> a) constructors
    let struct =
            CStructDef
                structName
                [ CTDTNormal (CTName "uint8_t") "variant"
                , CTDTUnion
                    (map (\(_, b, _) -> b) constructors)
                    "variants"
                ]
    return
        ( helperStructs
        , struct
        , foldl (\acc (_, _, (f1, f2, f3)) -> acc ++ [f1, f2, f3]) [] constructors
        , CFunctionDef
            ( CFunctionDec
                (CTPointer $ CTStruct structName)
                ("__Copy_" ++ structName)
                [(CTPointer $ CTStruct structName, "t")]
            )
            $ [ CSVarDec (CTPointer $ CTStruct structName) "n"
              , CSAssign (CIden "n") $
                    CEFunction
                        "gc_malloc"
                        [CERef . CEVar $ CIden "gc", CESizeof (CTStruct structName)]
              , CSAssignDeref (CIden "n") . CEDeref . CEVar $ CIden "t"
              ]
                ++ [ foldl
                        ( \acc (nD, d, _) ->
                            ( case (nD, d) of
                                (Just (CStructDef sN _, _), CTDTNormal _ n) ->
                                    CSIfElse
                                        (CEFunction ("__Is" ++ n ++ "_" ++ structName) [CEVar $ CIden "t"])
                                        [ CSAssign
                                            (CSCDot (CSCArr (CIden "n") "variants") n)
                                            $ CEFunction ("__Copy_" ++ sN) [CEVar $ CSCDot (CSCArr (CIden "t") "variants") n]
                                        , CSReturn . CEVar $ CIden "n"
                                        ]
                                (Nothing, CTDTNormal (CTPointer (CTName "void")) n) ->
                                    CSIfElse
                                        (CEFunction ("__Is" ++ n ++ "_" ++ structName) [CEVar $ CIden "t"])
                                        [CSReturn . CEVar $ CIden "n"]
                                (Nothing, CTDTNormal (CTPointer t) n) ->
                                    CSIfElse
                                        (CEFunction ("__Is" ++ n ++ "_" ++ structName) [CEVar $ CIden "t"])
                                        [ CSAssign
                                            (CSCDot (CSCArr (CIden "n") "variants") n)
                                            $ CEFunction ("__Copy_" ++ cTypeName t) [CEVar $ CSCDot (CSCArr (CIden "t") "variants") n]
                                        , CSReturn . CEVar $ CIden "n"
                                        ]
                                _ -> error "this really can't happen... I swear"
                            )
                                [acc]
                        )
                        (CSExpr $ CEFunction "exit" [CENum False "69"])
                        constructors
                   ]
        )
  where
    constructor :: Int -> String -> [(String, CType)] -> (String, [Type]) -> Except [CError] (Maybe (CStructDef, CFunctionDef), CTypeDefT, (CFunctionDef, CFunctionDef, CFunctionDef))
    constructor n structName _ (name, []) = do
        let ctdt = CTDTNormal (CTPointer $ CTName "void") $ '_' : name
        fs <- constructorToFunction structName n (Nothing, ctdt)
        return (Nothing, ctdt, fs)
    constructor n structName vL (name, [t]) = do
        t' <- typeToC t vL
        let ctdt = CTDTNormal t' $ '_' : name
        fs <- constructorToFunction structName n (Nothing, ctdt)
        return (Nothing, ctdt, fs)
    constructor n structName vL (name, ts) = do
        ts' <-
            mapM
                ( \(n, t) -> do
                    t' <- typeToC t vL
                    return $ CTDTNormal t' ('_' : show n)
                )
                $ zip [(0 :: Int) ..] ts
        let nameT = structName ++ '_' : name
        let ctdt = CTDTNormal (CTStruct $ '_' : tName ++ foldl (\str (_, t) -> str ++ "__" ++ cTypeName t) "" vL ++ '_' : name) ('_' : name)
        let copyF =
                CFunctionDef
                    ( CFunctionDec
                        (CTStruct nameT)
                        ("__Copy_" ++ nameT)
                        [(CTStruct nameT, "t")]
                    )
                    $ [CSVarDec (CTStruct nameT) "n"]
                        ++ map
                            ( \a -> case a of
                                CTDTNormal (CTPointer t) n' -> CSAssign (CSCDot (CIden "n") n') $ CEFunction ("__Copy_" ++ cTypeName t) [CEVar $ CSCDot (CIden "t") n']
                                _ -> error "can't happen realldasyfa"
                            )
                            ts'
                        ++ [CSReturn . CEVar $ CIden "n"]
        let struct = Just (CStructDef nameT ts', copyF)
        fs <- constructorToFunction structName n (fmap fst struct, ctdt)
        return (struct, ctdt, fs)

constructorToFunction :: String -> Int -> (Maybe CStructDef, CTypeDefT) -> Except [CError] (CFunctionDef, CFunctionDef, CFunctionDef)
constructorToFunction structName n (m, CTDTNormal t name) =
    let structT = CTStruct structName
     in let structTPtr = CTPointer $ CTStruct structName
         in case (m, t) of
                (Nothing, CTPointer (CTName "void")) -> do
                    let new =
                            CFunctionDef
                                ( CFunctionDec
                                    structTPtr
                                    ("_" ++ name ++ structName)
                                    []
                                )
                                [ CSVarDec structTPtr "new"
                                , CSAssign (CIden "new") $ CEFunction "gc_malloc" [CERef . CEVar $ CIden "gc", CESizeof structT]
                                , CSAssign (CSCArr (CIden "new") "variant") $ CENum False $ show n
                                , CSAssign (CSCDot (CSCArr (CIden "new") "variants") name) CENull
                                , CSReturn . CEVar $ CIden "new"
                                ]
                    let is =
                            CFunctionDef
                                ( CFunctionDec
                                    (CTName "bool")
                                    ("__Is" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSReturn
                                    $ CEInfix
                                        (CEVar (CSCArr (CIden "t") "variant"))
                                        CIEq
                                    $ CENum False
                                    $ show n
                                ]
                    let get =
                            CFunctionDef
                                ( CFunctionDec
                                    (CTPointer $ CTName "void")
                                    ("__Get" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSIf
                                    (CENot $ CEFunction ("__Is" ++ name ++ "_" ++ structName) [CEVar $ CIden "t"])
                                    [ CSExpr $ CEFunction "fprintf" [CEVar $ CIden "stderr", CEString $ "Value not " ++ name]
                                    , CSExpr $ CEFunction "exit" [CENum False "1"]
                                    ]
                                , CSReturn . CERef $ CEVar (CSCDot (CSCArr (CIden "t") "variants") name)
                                ]
                    return (new, is, get)
                (Just (CStructDef name' ts), CTStruct _) -> do
                    let new =
                            CFunctionDef
                                ( CFunctionDec
                                    structTPtr
                                    ("_" ++ name ++ structName)
                                    ( map
                                        ( \t' -> case t' of
                                            CTDTNormal t n -> (t, n)
                                            _ -> error "unreachable copium"
                                        )
                                        ts
                                    )
                                )
                                $ [ CSVarDec structTPtr "new"
                                  , CSAssign (CIden "new") $ CEFunction "gc_malloc" [CERef . CEVar $ CIden "gc", CESizeof structT]
                                  , CSAssign (CSCArr (CIden "new") "variant") $ CENum False $ show n
                                  ]
                                    ++ ( let ass = CSCDot (CSCDot (CSCArr (CIden "new") "variants") name)
                                          in map
                                                ( \t' -> case t' of
                                                    CTDTNormal _ n -> CSAssign (ass n) $ CEVar $ CIden n
                                                    _ -> error "this really can't happen, trust me bro"
                                                )
                                                ts
                                       )
                                    ++ [CSReturn . CEVar $ CIden "new"]
                    let is =
                            CFunctionDef
                                ( CFunctionDec
                                    (CTName "bool")
                                    ("__Is" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSReturn
                                    $ CEInfix
                                        (CEVar (CSCArr (CIden "t") "variant"))
                                        CIEq
                                    $ CENum False
                                    $ show n
                                ]
                    let get =
                            CFunctionDef
                                ( CFunctionDec
                                    (CTPointer $ CTStruct name')
                                    ("__Get" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSIf
                                    (CENot $ CEFunction ("__Is" ++ name ++ "_" ++ structName) [CEVar $ CIden "t"])
                                    [ CSExpr $ CEFunction "fprintf" [CEVar $ CIden "stderr", CEString $ "Value not " ++ name]
                                    , CSExpr $ CEFunction "exit" [CENum False "1"]
                                    ]
                                , CSReturn . CERef $ CEVar (CSCDot (CSCArr (CIden "t") "variants") name)
                                ]
                    return (new, is, get)
                (Nothing, _) -> do
                    let new =
                            CFunctionDef
                                ( CFunctionDec
                                    structTPtr
                                    ("_" ++ name ++ structName)
                                    [(t, "v")]
                                )
                                [ CSVarDec structTPtr "new"
                                , CSAssign (CIden "new") $ CEFunction "gc_malloc" [CERef . CEVar $ CIden "gc", CESizeof structT]
                                , CSAssign (CSCArr (CIden "new") "variant") $ CENum False $ show n
                                , CSAssign (CSCDot (CSCArr (CIden "new") "variants") name) $ CEVar $ CIden "v"
                                , CSReturn . CEVar $ CIden "new"
                                ]
                    let is =
                            CFunctionDef
                                ( CFunctionDec
                                    (CTName "bool")
                                    ("__Is" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSReturn
                                    $ CEInfix
                                        (CEVar (CSCArr (CIden "t") "variant"))
                                        CIEq
                                    $ CENum False
                                    $ show n
                                ]
                    let get =
                            CFunctionDef
                                ( CFunctionDec
                                    (case t of CTPointer _ -> t; _ -> CTPointer t)
                                    ("__Get" ++ name ++ "_" ++ structName)
                                    [(structTPtr, "t")]
                                )
                                [ CSIf
                                    (CENot $ CEFunction ("__Is" ++ name ++ "_" ++ structName) [CEVar $ CIden "t"])
                                    [ CSExpr $ CEFunction "fprintf" [CEVar $ CIden "stderr", CEString $ "Value not " ++ name]
                                    , CSExpr $ CEFunction "exit" [CENum False "1"]
                                    ]
                                , CSReturn $ CEVar (CSCDot (CSCArr (CIden "t") "variants") name)
                                ]
                    return (new, is, get)
                _ -> undefined

typeToC :: Type -> [(String, CType)] -> Except [CError] CType
typeToC (TSimple "Int") _ = return . CTPointer . CTName $ "int64_t"
typeToC (TSimple "Char") _ = return . CTPointer . CTName $ "char"
typeToC (TSimple "String") _ = return . CTPointer . CTStruct $ "_String"
typeToC (TSimple "Bool") _ = return . CTPointer . CTName $ "bool"
typeToC (TSimple name) _ = return . CTPointer . CTStruct $ '_' : name
typeToC (TComplex name tArgs) tL = do
    _ <-
        foldl
            ( \str tA -> do
                str' <- str
                tA' <- typeToC tA tL
                return $ str' ++ cTypeName tA'
            )
            (return "")
            tArgs
    return . CTPointer . CTStruct $ '_' : name ++ foldl (\str (_, t) -> str ++ "__" ++ cTypeName t) "" tL
typeToC (TVar name) tL = case name `lookup` tL of
    Just t -> return t
    Nothing -> throwError [TypeVarNotFound name]
typeToC TUnit _ = return . CTPointer . CTName $ "void"
typeToC _ _ = undefined
