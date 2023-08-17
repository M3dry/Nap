{-# LANGUAGE LambdaCase #-}

module Static where

import Language.Haskell.TH.Syntax (Lift)
import Parser (File (File), TopLevel (TFunction, TTypeDef), fileP)
import Parser.Type (TypeDef (tConstructors, tName))
import Static.Duplicates (duplicates)
import Static.Found (checkFunction, checkTypeDef)
import Text.Parsec (ParseError, parse)
import Util (intoTwo)
import Static.Error (Error)
import Parser.Function (fName)

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
        map fName fs
          <> concatMap
            (map fst . tConstructors)
            ts
      typeRefs = map tName ts
  duplicates fs ts
  foldl
    (\acc f -> acc >> checkFunction exprRefs typeRefs f)
    (Right ())
    fs
  foldl
    (\acc t -> acc >> checkTypeDef typeRefs t)
    (Right ())
    ts
  where
    getTopLevel = foldl (\acc (CheckFile files' tops) -> acc <> tops <> getTopLevel files') []
