module ToC.Expression where
import ToC.CDSL (CType, CBlock, CError)
import Control.Monad.Except (Except)
import Static.Typing (Expression')
import Control.Monad.Reader (ReaderT)
import ToC.Refs (Refs)

expressionToC :: Expression' -> [(String, Refs)] -> ReaderT [(String, CType)] (Except [CError]) CBlock
expressionToC = undefined
