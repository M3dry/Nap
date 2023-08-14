module Parser.ExprStmt where

import Parser.Literal
import Parser.Pattern
import Parser.Type
import Parser.Util
import Text.Parsec

type Block = [Statement]

blockP :: Parser Block
blockP = insideSquirly . many1 $ statementP <* spaces

data Statement
  = SLet Bool (Maybe Type) Pattern Expression
  | SAssignment IdenName Expression
  | SIf Expression Expression
  | SReturn Expression
  | SExpr Expression
  | SFor Pattern Expression Expression
  | SWhile Expression Expression
  deriving (Show)

statementP :: Parser Statement
statementP =
  insideParen statementP
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
    <|> ( SExpr
            <$> expressionP
            <* spaces
            <* char ';'
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
                   <*> (spaces *> patternP)
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
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

data Expression
  = ELiteral (Literal Expression)
  | EIfElse Expression Expression Expression
  | EMatch Expression [(Pattern, Maybe Expression, Expression)]
  | EFunction IdenName [Expression]
  | EVar IdenName
  | EBlock Block
  | EType Expression Type
  | EInfix Expression Infix Expression
  deriving (Show)

expressionP :: Parser Expression
expressionP =
  ( (expr' <* spaces)
      `chainl1` ( flip EInfix
                    <$> ( Mul <$ (char '*' *> spaces)
                            <|> (Sub <$ (char '/' *> spaces))
                        )
                )
  )
    `chainl1` ( flip EInfix
                  <$> ( (Add <$ (char '+' *> spaces))
                          <|> (Sub <$ (char '-' *> spaces))
                      )
              )
  where
    iden = idenNameP <|> typeNameP
    expr =
      insideParen expressionP
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
