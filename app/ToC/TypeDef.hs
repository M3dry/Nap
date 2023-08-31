module ToC.TypeDef where

import Control.Monad.Except (Except, MonadError (throwError))
import Parser.Type
  ( Type
      ( TComplex,
        TSimple,
        TUnit,
        TVar
      ),
    TypeDef
      ( TypeDef,
        tConstructors,
        tName,
        tVars
      ),
  )
import ToC.CDSL
  ( CError
      ( TypeVarNotFound,
        VarLengthsMismatch
      ),
    CStructDef (CStructDef),
    CType (CTName, CTPointer, CTStruct),
    CTypeDefT (CTDTNormal, CTDTStruct, CTDTUnion),
    cTypeName,
  )

typeDefToC :: TypeDef -> [CType] -> Except [CError] CStructDef
typeDefToC TypeDef {tName, tVars, tConstructors} cVars = do
  varLookup <- do
    let tVLen = length tVars
    let cVLen = length cVars
    if tVLen == cVLen
      then return $ zip tVars cVars
      else throwError [VarLengthsMismatch tVLen cVLen]
  constructors <- mapM (constructor varLookup) tConstructors
  return $
    CStructDef
      ("_" ++ tName ++ foldl (\str cVar -> str ++ "__" ++ cTypeName cVar) "" cVars)
      [ CTDTNormal (CTName "uint8_t") "variant",
        CTDTUnion
          constructors
          "variants"
      ]
  where
    constructor :: [(String, CType)] -> (String, [Type]) -> Except [CError] CTypeDefT
    constructor _ (name, []) = return $ CTDTNormal (CTPointer $ CTName "void") $ '_' : name
    constructor vL (name, [t]) = do
      t' <- typeToC t vL
      return $ CTDTNormal t' $ '_' : name
    constructor vL (name, ts) = do
      ts' <-
        mapM
          ( \(n, t) -> do
              t' <- typeToC t vL
              return $ CTDTNormal t' ('_' : show n)
          )
          $ zip [(0 :: Int) ..] ts
      return $ CTDTStruct ts' $ '_' : name

typeToC :: Type -> [(String, CType)] -> Except [CError] CType
typeToC (TSimple "Int") _ = return . CTName $ "int64_t"
typeToC (TSimple "Char") _ = return . CTName $ "char"
typeToC (TSimple "String") _ = return . CTStruct . CTName $ "_String"
typeToC (TSimple "Bool") _ = return . CTName $ "bool"
typeToC (TSimple name) _ = return . CTStruct . CTName $ '_' : name
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
  return . CTStruct . CTName $ ('_' : name) ++ "__" ++ tAs
typeToC (TVar name) tL = case name `lookup` tL of
  Just t -> return t
  Nothing -> throwError [TypeVarNotFound name]
typeToC TUnit _ = return . CTPointer . CTName $ "void"
typeToC _ _ = undefined
