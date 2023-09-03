{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy qualified as L
import Parser
import Static (check, fileToCheckFile, fileifyFile)
import Text.Parsec
import Text.Pretty.Simple (pShow)
import ToC (compile)
import ToC.CDSL (ToString(toString))

main :: IO ()
main = do
  input <- getContents
  let file = case parse fileP "" input of
        Right f -> f
        Left err -> error $ show err
  fileifyied <- fileifyFile True file
  let checkFile = case fileToCheckFile fileifyied of
        Right f -> f
        Left err -> error $ show err
  putStrLn . L.unpack $ pShow checkFile
  putStrLn ""
  putStrLn . L.unpack . pShow . check $ checkFile
  putStrLn ""
  let w = check checkFile
  putStrLn . L.unpack . pShow $ w
  case w of
    Right fs -> putStrLn . L.unpack . pShow $ case compile (fs, undefined) of
        Right fs' -> Right $ map toString fs'
        Left err -> Left err
    Left _ -> return ()
