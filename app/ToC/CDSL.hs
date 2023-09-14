module ToC.CDSL where

import Parser.ExprStmt (Infix (IAdd, IAnd, IDiv, IEq, IGE, IGT, ILE, ILT, IMul, INotEq, IOr, ISub))
import ToC.Util (join)

data CError
    = VarLengthsMismatch Int Int
    | TypeVarNotFound String
    | NotFound String
    | NotMut String
    deriving (Show)

class ToString a where
    toString :: a -> String

data CType
    = CTName String
    | CTPointer CType
    | CTStruct String
    deriving (Show, Eq)

isVoidPtr :: CType -> Bool
isVoidPtr (CTPointer (CTName "void")) = True
isVoidPtr _ = False

cTypeName :: CType -> String
cTypeName (CTName name) = name
cTypeName (CTPointer name) = cTypeName name ++ "_ptr"
cTypeName (CTStruct name) = name

instance ToString CType where
    toString (CTName name) = name
    toString (CTPointer t) = toString t ++ "*"
    toString (CTStruct t) = "struct " ++ t

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
        [ CTDTNormal (CTName "uint64_t") "length"
        , CTDTNormal (CTPointer t) "data"
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
            ++ join (map (\(t, n) -> toString t ++ " " ++ n) params) ", "
            ++ ")"

data CFunctionDef = CFunctionDef CFunctionDec CBlock

instance ToString CFunctionDef where
    toString (CFunctionDef dec body) = toString dec ++ " " ++ toString body

type CBlock = [CStatement]

instance ToString CBlock where
    toString block = "{" ++ foldl (\acc b -> acc ++ toString b) "" block ++ "}"

data CIden
    = CSCDot CIden String
    | CSCArr CIden String
    | CIden String

instance ToString CIden where
    toString (CSCDot n n') = toString n ++ "." ++ n'
    toString (CSCArr n n') = toString n ++ "->" ++ n'
    toString (CIden n) = n

data CStatement
    = CSVarDec CType String
    | CSAssign CIden CExpression
    | CSAssignDeref CIden CExpression
    | CSIfElse CExpression CBlock CBlock
    | CSIf CExpression CBlock
    | CSReturn CExpression
    | CSExpr CExpression

instance ToString CStatement where
    toString (CSVarDec ct name) = toString ct ++ " " ++ name ++ ";"
    toString (CSAssign name expr) = toString name ++ " = " ++ toString expr ++ ";"
    toString (CSAssignDeref name expr) = "*" ++ toString name ++ " = " ++ toString expr ++ ";"
    toString (CSIfElse cond true false) =
        "if ("
            ++ toString cond
            ++ ") "
            ++ toString true
            ++ " else "
            ++ toString false
    toString (CSIf cond true) = "if (" ++ toString cond ++ ") " ++ toString true
    toString (CSReturn expr) = "return " ++ toString expr ++ ";"
    toString (CSExpr expr) = toString expr ++ ";"

data CExpression
    = CEVar CIden
    | CENum Bool String
    | CEInfix CExpression CInfix CExpression
    | CENull
    | CEFunction String [CExpression]
    | CESizeof CType
    | CERef CExpression
    | CEDeref CExpression
    | CENot CExpression
    | CEString String

instance ToString CExpression where
    toString (CEVar name) = toString name
    toString (CENum neg digs) =
        if neg
            then '-' : digs
            else digs
    toString (CEInfix rhs infx lhs) =
        paren (toString rhs)
            ++ " "
            ++ toString infx
            ++ " "
            ++ paren (toString lhs)
      where
        paren x = "(" ++ x ++ ")"
    toString CENull = "NULL"
    toString (CEFunction name args) = name ++ "(" ++ join (map toString args) ", " ++ ")"
    toString (CESizeof t) = "sizeof(" ++ toString t ++ ")"
    toString (CERef expr) = "&" ++ toString expr
    toString (CEDeref expr) = "*" ++ toString expr
    toString (CENot expr) = "!" ++ toString expr
    toString (CEString str) = show str

data CInfix
    = CIAdd
    | CISub
    | CIMul
    | CIDiv
    | CILT
    | CILE
    | CIGT
    | CIGE
    | CIEq
    | CINotEq
    | CIAnd
    | CIOr

infixToCInfix :: Infix -> CInfix
infixToCInfix IAdd = CIAdd
infixToCInfix ISub = CISub
infixToCInfix IMul = CIMul
infixToCInfix IDiv = CIDiv
infixToCInfix ILT = CILT
infixToCInfix ILE = CILE
infixToCInfix IGT = CIGT
infixToCInfix IGE = CIGE
infixToCInfix IEq = CIEq
infixToCInfix INotEq = CINotEq
infixToCInfix IAnd = CIAnd
infixToCInfix IOr = CIOr

instance ToString CInfix where
    toString CIAdd = "+"
    toString CISub = "-"
    toString CIMul = "*"
    toString CIDiv = "/"
    toString CILT = "<"
    toString CILE = "<="
    toString CIGT = ">"
    toString CIGE = ">="
    toString CIEq = "=="
    toString CINotEq = "!="
    toString CIAnd = "&&"
    toString CIOr = "||"
