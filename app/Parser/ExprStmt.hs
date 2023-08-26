module Parser.ExprStmt where

import Language.Haskell.TH.Syntax (Lift)
import Parser.Literal
import Parser.Pattern
import Parser.Type
import Parser.Util
import Text.Parsec

type Block = [Statement]

blockP :: Parser Block
blockP = insideSquirly . many1 $ statementP <* spaces

data Statement
    = SLet Bool (Maybe Type) IdenName Expression
    | SAssignment IdenName Expression
    | SIf Expression Expression
    | SReturn Expression
    | SExpr Expression
    | SExprRet Expression
    | SFor Pattern Expression Expression
    | SWhile Expression Expression
    deriving (Show, Lift)

statementP :: Parser Statement
statementP =
    (notFollowedBy (string "()") *> insideParen statementP)
        <|> try
            ( SAssignment
                <$> idenNameP
                <*> ( spaces
                        *> char '='
                        *> spaces
                        *> expressionP
                        <* spaces
                        <* char ';'
                    )
            )
        <|> ( (\expr semi -> if semi then SExpr expr else SExprRet expr)
                <$> expressionP
                <* spaces
                <*> option False (True <$ char ';')
            )
        <|> ( string "if"
                *> ( SIf
                        <$> (spaces *> insideParen expressionP)
                        <*> (spaces *> insideSquirly expressionP)
                   )
            )
        <|> ( string "let"
                *> ( SLet
                        <$> ( spaces
                                *> ( try (True <$ string "mut")
                                        <|> pure False
                                   )
                                <* spaces
                            )
                        <*> optionMaybe (char ':' *> spaces *> typeP)
                        <*> (spaces *> idenNameP)
                        <*> ( spaces
                                *> char '='
                                *> spaces
                                *> expressionP
                                <* spaces
                                <* char ';'
                            )
                   )
            )
        <|> ( string "return"
                *> ( SReturn
                        <$> ( spaces
                                *> expressionP
                                <* spaces
                                <* char ';'
                            )
                   )
            )
        <|> ( string "for"
                *> ( SFor
                        <$> (spaces *> patternP)
                        <*> ( spaces
                                *> string "in"
                                *> spaces
                                *> insideParen expressionP
                            )
                        <*> (spaces *> expressionP)
                   )
            )
        <|> ( string "while"
                *> ( SWhile
                        <$> (spaces *> insideParen expressionP)
                        <*> ( spaces
                                *> expressionP
                            )
                   )
            )

data Infix
    = IAdd
    | ISub
    | IMul
    | IDiv
    | ILT
    | ILE
    | IGT
    | IGE
    | IEq
    | INotEq
    | IAnd
    | IOr
    deriving (Show, Lift)

data InfixType
    = Numeric
    | Boolean

infixType :: Infix -> (InfixType, InfixType, InfixType)
infixType i = case i of
    IAdd -> allNum
    ISub -> allNum
    IMul -> allNum
    IDiv -> allNum
    ILT -> argNumBool
    ILE -> argNumBool
    IGT -> argNumBool
    IGE -> argNumBool
    IEq -> allBool
    INotEq -> allBool
    IAnd -> allBool
    IOr -> allBool
  where
    allNum = (Numeric, Numeric, Numeric)
    allBool = (Boolean, Boolean, Boolean)
    argNumBool = (Numeric, Numeric, Boolean)

infixP :: Parser Expression -> Parser Expression
infixP expP =
    ( ( ( (expP <* spaces)
            `chainl1` mulDivP
        )
            `chainl1` addSubP
      )
        `chainl1` logicP
    )
        `chainl1` compP
  where
    mulDivP =
        flip EInfix
            <$> ( IMul <$ (char '*' *> spaces)
                    <|> (ISub <$ (char '/' *> spaces))
                )
    addSubP =
        flip EInfix
            <$> ( (IAdd <$ (char '+' *> spaces))
                    <|> (ISub <$ (char '-' *> spaces))
                )
    logicP =
        flip EInfix
            <$> ( (ILE <$ try (string "<=" *> spaces))
                    <|> (IGE <$ try (string ">=" *> spaces))
                    <|> (ILT <$ (char '<' *> spaces))
                    <|> (IGT <$ (char '>' *> spaces))
                )
    compP =
        flip EInfix
            <$> ( (IEq <$ try (string "==" *> spaces))
                    <|> (INotEq <$ (string "!=" *> spaces))
                    <|> (IAnd <$ (string "&&" *> spaces))
                    <|> (IOr <$ (string "||" *> spaces))
                )

data Expression
    = ELiteral (Literal Expression)
    | EIfElse Expression Expression Expression
    | EMatch Expression [(Pattern, Maybe Expression, Expression)]
    | EFunction IdenName [Expression]
    | EVar IdenName
    | EBlock Block
    | EType Expression Type
    | EInfix Expression Infix Expression
    deriving (Show, Lift)

expressionP :: Parser Expression
expressionP = infixP expr'
  where
    iden = idenNameP <|> typeNameP
    expr =
        try (ELiteral LUnit <$ string "()")
            <|> insideParen expressionP
            <|> (ELiteral <$> literalP expressionP)
            <|> try
                ( EFunction
                    <$> iden
                    <*> ( spaces
                            *> insideParen
                                ( (expressionP <* spaces)
                                    `sepEndBy` (char ',' *> spaces)
                                )
                        )
                )
            <|> try (EVar <$> iden)
            <|> (EBlock <$> blockP)
            <|> ( string "if"
                    *> ( EIfElse
                            <$> (spaces *> insideParen expressionP)
                            <*> (spaces *> insideSquirly expressionP)
                            <*> (spaces *> string "else" *> spaces *> insideSquirly expressionP)
                       )
                )
            <|> ( string "match"
                    *> ( EMatch
                            <$> (spaces *> insideParen expressionP)
                            <*> ( spaces
                                    *> insideSquirly
                                        ( spaces
                                            *> ( ( (,,)
                                                    <$> (patternP <* spaces)
                                                    <*> optionMaybe (string "if" *> spaces *> expressionP)
                                                    <*> ( spaces
                                                            *> string "=>"
                                                            *> spaces
                                                            *> expressionP
                                                        )
                                                 )
                                                    `sepEndBy1` (char ',' *> spaces)
                                               )
                                        )
                                )
                       )
                )
    expr' =
        try
            ( EType
                <$> expr
                <*> ( spaces
                        *> char ':'
                        *> spaces
                        *> typeP
                    )
            )
            <|> expr
