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
    CExpression (CEFunction, CEInfix, CENot, CENull, CENum, CERef, CESizeof, CEString, CEVar),
    CFunctionDec (CFunctionDec),
    CFunctionDef (CFunctionDef),
    CIden (CIden, CSCArr, CSCDot),
    CInfix (CIEq),
    CStatement (CSAssign, CSExpr, CSIf, CSReturn, CSVarDec),
    CStructDef (CStructDef),
    CType (CTName, CTPointer, CTStruct),
    CTypeDefT (CTDTNormal, CTDTUnion),
    cTypeName,
 )

typeDefToC :: TypeDef -> [CType] -> Except [CError] ([CStructDef], CStructDef, [CFunctionDef])
typeDefToC TypeDef{tName, tVars, tConstructors} cVars = do
    varLookup <- do
        let tVLen = length tVars
        let cVLen = length cVars
        if tVLen == cVLen
            then return $ zip tVars cVars
            else throwError [VarLengthsMismatch tVLen cVLen]
    let structName = "_" ++ tName ++ foldl (\str cVar -> str ++ "__" ++ cTypeName cVar) "" cVars
    constructors <- zipWithM (\t i -> constructor i structName varLookup t) tConstructors [1..]
    let helperStructs =
            foldl
                ( \acc m -> case m of
                    Just (CStructDef name ts) -> CStructDef name ts : acc
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
    return (helperStructs, struct, foldl (\acc (_, _, (f1, f2, f3)) -> acc ++ [f1, f2, f3]) [] constructors)
  where
    constructor :: Int -> String -> [(String, CType)] -> (String, [Type]) -> Except [CError] (Maybe CStructDef, CTypeDefT, (CFunctionDef, CFunctionDef, CFunctionDef))
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
        let ctdt = CTDTNormal (CTStruct tName) ('_' : name)
        let struct = Just $ CStructDef nameT ts'
        fs <- constructorToFunction structName n (struct, ctdt)
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
                (Just (CStructDef name' ts), CTStruct struct) -> do
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
                                , CSReturn . CERef $ CEVar (CSCDot (CSCArr (CIden "t") "variants") name)
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
    tAs <-
        foldl
            ( \str tA -> do
                str' <- str
                tA' <- typeToC tA tL
                return $ str' ++ cTypeName tA'
            )
            (return "")
            tArgs
    return . CTPointer . CTStruct $ ('_' : name) ++ "__" ++ tAs
typeToC (TVar name) tL = case name `lookup` tL of
    Just t -> return t
    Nothing -> throwError [TypeVarNotFound name]
typeToC TUnit _ = return . CTPointer . CTName $ "void"
typeToC _ _ = undefined
