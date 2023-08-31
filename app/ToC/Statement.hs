module ToC.Statement where

import Control.Monad.Except (Except, MonadTrans (lift))
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Maybe (fromMaybe)
import Static.Typing (Block', Statement' (SLet'), Typed (Typed))
import ToC.CDSL (CBlock, CError, CStatement (CSVarDec), CType)
import ToC.Expression (expressionToC)
import ToC.Refs (Refs (RShadowed, RTodo, RVar))
import ToC.TypeDef (typeToC)

blockToC :: Block' -> [(String, Refs)] -> ReaderT [(String, CType)] (Except [CError]) CBlock
blockToC [] _ = return []
blockToC (bock : block) refs = do
  tVars <- ask
  case bock of
    SLet' mut name (Typed expr t) -> do
      t' <- lift $ typeToC t tVars
      let (refs', nameM) = case name `lookup` refs of
            Just _ ->
              let name' = '_' : name ++ name
               in ( (name', RVar mut t')
                      : map (\(n, a') -> (n, if n == name then RShadowed name' else a')) refs,
                    Just name'
                  )
            Nothing -> ((name, RVar mut t') : refs, Nothing)
      let dec = CSVarDec t' $ '_' : fromMaybe name nameM
      expr' <- expressionToC expr refs
      block' <- blockToC block refs'
      return $ dec : expr' ++ block'
