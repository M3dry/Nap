{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser
import Text.Parsec
import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as L

main :: IO ()
main = interact $ L.unpack . (<>"\n") . pShow . parse fileP ""
