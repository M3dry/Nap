{-# LANGUAGE TemplateHaskell #-}

module Base where

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH.Syntax (Lift (lift))
import Parser (fileP)
import Static (CheckFile, fileToCheckFile, fileifyFile)
import Text.Parsec (parse)

-- prelude :: CheckFile
-- prelude =
--   $( case parse fileP "" $ unpack $(embedFile "base/Prelude.dg") of
--        Right ok -> lift $ case fileToCheckFile . unsafePerformIO $ fileifyFile False ok of
--          Right ok' -> ok'
--          Left err -> error $ show err
--        Left err -> error $ show err
--    )
