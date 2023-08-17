{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy qualified as L
import Parser
import Static (check, fileToCheckFile, fileifyFile)
import Text.Parsec
import Text.Pretty.Simple (pShow)

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
  putStrLn . L.unpack . pShow . check $ checkFile
