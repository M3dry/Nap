module ToC where

import Parser.Type (TypeDef (TypeDef, tConstructors, tName, tVars))

compileTypeDef :: TypeDef -> String
compileTypeDef TypeDef{tName, tVars, tConstructors} = undefined
