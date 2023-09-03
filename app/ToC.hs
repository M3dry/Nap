module ToC where

import ToC.Function (functionToC)
import ToC.CDSL (CError, CFunctionDef)
import Control.Monad.Except (Except, runExcept)
import Static.Typing (FunctionTyped)
import Parser.Type (TypeDef)
import Control.Monad.Reader (ReaderT(runReaderT))

compile :: ([FunctionTyped], [TypeDef]) -> Either [CError] [CFunctionDef]
compile (fs, ts) = do
    runExcept $ runReaderT (mapM functionToC fs) []
