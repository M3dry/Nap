{-# LANGUAGE LambdaCase #-}

module Static where

import Data.Map qualified as M
import Language.Haskell.TH.Syntax (Lift)
import Parser (File (File), TopLevel (TFunction, TTypeDef), fileP)
import Parser.Function (fName)
import Parser.Type (Type (TComplex, TVar), TypeDef (tConstructors, tName, tVars))
import Static.Error (Error)
import Static.Typing (typeCheckFunction)
import Text.Parsec (ParseError, parse)
import Util (Treither (First, Second), intoTwo)

data CheckFile = CheckFile [CheckFile] [TopLevel]
  deriving (Show, Lift)

fileifyFile :: Bool -> File -> IO File
fileifyFile prelude (File imports file) = do
  imports' <- mapM readFile $ (if prelude then ("base/Prelude.dg" :) else id) imports
  return $ File imports' file

fileToCheckFile :: File -> Either [ParseError] CheckFile
fileToCheckFile (File files file) =
  let (errs, files') = intoTwo id $ map (parse fileP "") files
   in if null errs
        then
          let (errs', checkFiles) = intoTwo id $ map fileToCheckFile files'
           in if null errs' || null (head errs')
                then Right $ CheckFile checkFiles file
                else Left $ concat errs'
        else Left errs

check :: CheckFile -> Either [Error] ()
check (CheckFile imports file) = do
  let (fs, ts) =
        intoTwo
          ( \case
              TFunction f -> Left f
              TTypeDef t -> Right t
          )
          (file <> getTopLevel imports)
      exprRefs =
        M.fromList $
          map (\f -> (fName f, First f)) fs
            <> concatMap
              ( \t ->
                  map
                    (\(name, sig) -> (name, Second $ typeFromConstructor t sig))
                    $ tConstructors t
              )
              ts
      typeRefs = M.fromList $ map (\t -> (tName t, t)) ts
  foldl
    (\acc f -> acc >> typeCheckFunction exprRefs typeRefs f)
    (Right ())
    fs
  where
    getTopLevel = foldl (\acc (CheckFile files' tops) -> acc <> tops <> getTopLevel files') []
    typeFromConstructor :: TypeDef -> [Type] -> ([Type], Type)
    typeFromConstructor t sig =
      (sig, TComplex (tName t) $ map TVar $ tVars t)
