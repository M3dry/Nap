module Parser.Type where

import Parser.Util
import Text.Parsec

data Type
  = TSimple TypeName
  | TComplex TypeName [Type]
  | TVar IdenName
  deriving (Show)

typeP :: Parser Type
typeP =
  try
    ( TComplex
        <$> typeNameP
        <*> ( spaces
                *> insideParen
                  (many $ typeP <* spaces)
            )
    )
    <|> (TSimple <$> typeNameP)
    <|> (TVar <$> idenNameP)

data TypeDef = TypeDef
  { tName :: TypeName,
    tVars :: [IdenName],
    tConstructors :: [(TypeName, [Type])]
  }
  deriving (Show)

typeDefP :: Parser TypeDef
typeDefP =
  TypeDef
    <$> ( string
            "type"
            *> spaces
            *> typeNameP
        )
    <*> ( spaces
            *> insideParen
              ( (idenNameP <* spaces)
                  `sepEndBy` (char ',' *> spaces)
              )
        )
    <*> ( spaces
            *> char '='
            *> spaces
            *> insideSquirly
              ( ( (,)
                    <$> typeNameP
                    <*> ( spaces
                            *> insideParen
                              ( (typeP <* spaces)
                                  `sepEndBy` (char ',' *> spaces)
                              )
                        )
                )
                  `sepEndBy1` (char ',' *> spaces)
              )
        )
