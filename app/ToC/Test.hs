module ToC.Test where

import Control.Monad.Except (runExcept)
import Parser.Type
import ToC.CDSL (CFunctionDec (CFunctionDec), CFunctionDef (CFunctionDef), CStructDef (CStructDef), CType (CTName, CTPointer, CTStruct), ToString (toString))
import ToC.TypeDef
import ToC.Util (join)

opt = TypeDef "Opt" ["a"] [("Just", [TVar "a"]), ("Nothing", [])]

list = TypeDef "List" ["a"] [("List", [TVar "a", TComplex "List" [TVar "a"]]), ("Empty", [])]

list' = runExcept $ typeDefToC list [CTPointer $ CTName "int64_t"]

opt' = runExcept $ typeDefToC opt [CTPointer $ CTStruct "_List__int64_t_ptr"]

run :: IO ()
run = run' [opt', list']

run' t =
    let t' = map unwrap t
     in do
            sequence_ $ sDecs t'
            sequence_ $ fDecs t'
            sequence_ $ bods t'
  where
    unwrap (Left e) = error $ show e
    unwrap (Right r) = r
    sDecs =
        map
            ( \(sts, s, fs, copyF) -> do
                mapM_ (putStrLn . structDec . fst) sts
                putStrLn $ structDec s
            )
    fDecs =
        map
            ( \(sts, s, fs, copyF) -> do
                mapM_ (putStrLn . printD . dec . snd) sts
                putStrLn . printD $ dec copyF
                mapM_ (putStrLn . printD . dec) fs
            )
    bods =
        map
            ( \(sts, s, fs, copyF) -> do
                mapM_ (putStrLn . toString . fst) sts
                putStrLn $ toString s

                mapM_ (putStrLn . toString . snd) sts
                putStrLn $ toString copyF
                mapM_ (putStrLn . toString) fs
            )
    dec (CFunctionDef d _) = d
    structDec (CStructDef n _) = "struct " ++ n ++ ";"
    printD (CFunctionDec ret name sig) = toString ret ++ " " ++ name ++ "(" ++ join (map (toString . fst) sig) ", " ++ ");"
