module ToC.CDSL where
import ToC.Util (join)

data CError
  = VarLengthsMismatch Int Int
  | TypeVarNotFound String
  deriving (Show)

class ToString a where
  toString :: a -> String

data CType
  = CTName String
  | CTPointer CType
  | CTStruct CType
  deriving (Show)

cTypeName :: CType -> String
cTypeName (CTName name) = name
cTypeName (CTPointer name) = cTypeName name ++ "_ptr"
cTypeName (CTStruct name) = cTypeName name

instance ToString CType where
  toString (CTName name) = name
  toString (CTPointer t) = toString t ++ "*"
  toString (CTStruct t) = "struct " ++ toString t

data CStructDef = CStructDef String [CTypeDefT]

instance ToString CStructDef where
  toString (CStructDef name ts) = "struct " ++ name ++ "{" ++ foldl (\str t -> str ++ toString t) "" ts ++ "};"

data CTypeDefT
  = CTDTNormal CType String
  | CTDTStruct [CTypeDefT] String
  | CTDTUnion [CTypeDefT] String

instance ToString CTypeDefT where
  toString (CTDTNormal t name) = toString t ++ " " ++ name ++ ";"
  toString (CTDTStruct ts name) = "struct {" ++ foldl (\str t -> str ++ toString t) "" ts ++ "} " ++ name ++ ";"
  toString (CTDTUnion ts name) = "union {" ++ foldl (\str t -> str ++ toString t) "" ts ++ "} " ++ name ++ ";"

vector :: CType -> CStructDef
vector t =
  CStructDef
    ("_Vector__" ++ cTypeName t)
    [ CTDTNormal (CTName "uint64_t") "length",
      CTDTNormal (CTPointer t) "data"
    ]

string :: CStructDef
string = undefined

data CFunctionDec = CFunctionDec CType String [(CType, String)]

instance ToString CFunctionDec where
  toString (CFunctionDec retT name params) =
    toString retT
      ++ " "
      ++ name
      ++ "("
      ++ join (map (\(t, n) -> toString t ++ " " ++ n)  params) ", "
      ++ ")"

data CFunctionDef = CFunctionDef CFunctionDec CBlock

instance ToString CFunctionDef where
  toString (CFunctionDef dec body) = toString dec ++ " " ++ toString body

type CBlock = [CStatement]

instance ToString CBlock where
  toString = undefined

data CStatement = CSVarDec CType String

instance ToString CStatement where
  toString = undefined
