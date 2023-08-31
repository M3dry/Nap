module ToC.Function where

import Control.Monad.Except (Except)
import Control.Monad.Reader (MonadReader (ask), MonadTrans (lift), ReaderT (runReaderT))
import Data.Map qualified as M
import Parser.Function
  ( Function,
    Function'
      ( Function,
        fBody,
        fName,
        fReturn,
        fSignature
      ),
  )
import Static.Typing (FunctionTyped)
import ToC.CDSL
  ( CError,
    CFunctionDec (CFunctionDec),
    CFunctionDef (CFunctionDef),
    CType (CTName, CTPointer),
  )
import ToC.Statement (blockToC)
import ToC.TypeDef (typeToC)

functionToC :: FunctionTyped -> [(String, ())] -> ReaderT [(String, CType)] (Except [CError]) CFunctionDef
functionToC Function {fName, fSignature, fReturn, fBody} _ = do
  tVars <- ask
  returnT <-
    maybe
      (return $ CTName "void")
      (lift . (`typeToC` tVars))
      fReturn
  signature <-
    lift $
      mapM
        ( \(_, name, t) -> do
            t' <- typeToC t tVars
            return
              ( CTPointer t',
                '_' : name
              )
        )
        fSignature
  body <- lift $ runReaderT (blockToC fBody []) tVars
  return $
    CFunctionDef
      ( CFunctionDec
          returnT
          ('_' : fName)
          signature
      )
      body
