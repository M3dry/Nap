module ToC.Test where

import ToC.TypeDef
import Parser.Type
import Control.Monad.Except (runExcept)
import ToC.CDSL (ToString(toString), CType (CTName))
import Text.Pretty.Simple (pPrint)

emp = TypeDef "Opt" ["a"] [("Just", [TVar "a"]), ("None", [])]

toc = runExcept $ typeDefToC emp [CTName "int64_t"]

run :: IO ()
run = case toc of
        Left err -> error $ show err
        Right (sts, s, fs) -> do
            pPrint $ map toString sts
            pPrint $ toString s
            pPrint $ map toString fs
