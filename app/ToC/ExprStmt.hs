module ToC.ExprStmt where

import Control.Monad (zipWithM)
import Control.Monad.Except (Except, MonadError (throwError), MonadTrans (lift), unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import Data.Maybe (fromMaybe)
import Parser.Literal (Literal (LArray, LChar, LNum, LString, LUnit))
import Static.Typing (Block', Expression' (EBlock', EFunction', EIfElse', EInfix', ELiteral', EMatch', EVar'), Statement' (SAssignment', SExpr', SExprRet', SFor', SIf', SLet', SReturn', SWhile'), Typed (Typed))
import ToC.CDSL (CBlock, CError (NotFound, NotMut), CExpression (CEFunction, CEInfix, CENull, CENum, CEVar), CStatement (CSAssign, CSAssignDeref, CSExpr, CSIf, CSIfElse, CSReturn, CSVarDec), CType, infixToCInfix, isVoidPtr, CIden (CIden))
import ToC.TypeDef (typeToC)

data Refs
    = RShadowed String
    | RVar Bool CType

findVarRefs :: String -> [(String, Refs)] -> Except [CError] (String, Bool, CType)
findVarRefs name refs = case name `lookup` refs of
    Just (RShadowed name') -> findVarRefs name' refs
    Just (RVar mut t) -> return (name, mut, t)
    Nothing -> throwError [NotFound name]

blockToC :: Maybe (CExpression -> CStatement) -> Block' -> [(String, Refs)] -> ReaderT [(String, CType)] (Except [CError]) CBlock
blockToC _ [] _ = return []
blockToC assignF (bock : block) refs = do
    tVars <- ask
    case bock of
        SLet' mut name (Typed expr t) -> do
            t' <- lift $ typeToC t tVars
            let (refs', nameM) = case name `lookup` refs of
                    Just _ ->
                        let name' = '_' : name ++ name
                         in ( (name', RVar mut t')
                                : map (\(n, a') -> (n, if n == name then RShadowed name' else a')) refs
                            , Just name'
                            )
                    Nothing -> ((name, RVar mut t') : refs, Nothing)
            let name' = '_' : fromMaybe name nameM
            let dec = CSVarDec t' name'
            expr' <- assignToC (whenVoidPtr t' name') expr refs
            block' <- blockToC assignF block refs'
            return $ dec : expr' ++ block'
        SAssignment' name (Typed expr _) -> do
            (name', mut, t) <- lift $ name `findVarRefs` refs
            unless mut $ throwError [NotMut name']
            assig <- assignToC (whenVoidPtr t name') expr refs
            block' <- blockToC assignF block refs
            return $ assig ++ block'
        SIf' (Typed cond condT) (Typed true trueT) -> do
            condCT <- lift $ typeToC condT tVars
            let condName = "__cond"
            let condDec = CSVarDec condCT condName
            condBlock <- assignToC (CSAssign $ CIden condName) cond refs
            trueBlock <- blockToC assignF [SExpr' (Typed true trueT)] refs
            return $
                condDec
                    : condBlock
                    ++ [CSIf (CEVar $ CIden condName) trueBlock]
        SReturn' (Typed expr exprT) -> do
            exprCT <- lift $ typeToC exprT tVars
            let retName = "__return"
            let retDec = CSVarDec exprCT retName
            retBlock <- assignToC (whenVoidPtr exprCT retName) expr refs
            return $
                retDec
                    : retBlock
                    ++ [CSReturn $ CEVar $ CIden retName]
        SExpr' (Typed expr _) -> assignToC CSExpr expr refs
        SExprRet' (Typed expr exprT) -> do
            case assignF of
                Just a -> assignToC a expr refs
                Nothing -> do
                    exprCT <- lift $ typeToC exprT tVars
                    let retName = "__return"
                    let retDec = CSVarDec exprCT retName
                    retBlock <- assignToC (whenVoidPtr exprCT retName) expr refs
                    return $
                        retDec
                            : retBlock
                            ++ [CSReturn $ CEVar $ CIden retName]

-- SFor' iterName (Typed iterExpr iterExprT) (Typed body bodyT) -> do
--     undefined
-- SWhile' (Typed cond condT) (Typed body bodyT) -> do
--     undefined

assignToC :: (CExpression -> CStatement) -> Expression' -> [(String, Refs)] -> ReaderT [(String, CType)] (Except [CError]) CBlock
assignToC assignF expression refs = do
    tVars <- ask
    case expression of
        ELiteral' (Typed literal _literalT) ->
            case literal of
                LNum neg digs -> return [assignF $ CENum neg digs]
                LArray _ -> undefined
                LString string -> undefined
                LChar char -> undefined
                LUnit -> return [assignF CENull]
        EIfElse' (Typed cond condT) (Typed true trueT) (Typed false falseT) -> do
            condCT <- lift $ typeToC condT tVars
            let condName = "__cond"
            let condDec = CSVarDec condCT condName
            condBlock <- assignToC (CSAssign $ CIden condName) cond refs
            trueBlock <- blockToC (pure assignF) [SExpr' (Typed true trueT)] refs
            falseBlock <- blockToC (pure assignF) [SExpr' (Typed false falseT)] refs
            return $
                condDec
                    : condBlock
                    ++ [ CSIfElse
                            (CEVar $ CIden condName)
                            trueBlock
                            falseBlock
                       ]
        EMatch' _ _ -> undefined
        EFunction' name args -> do
            let args' =
                    zipWith
                        (\i _ -> "__arg" ++ show i)
                        [(1 :: Int) ..]
                        args
            argsB <-
                concat
                    <$> zipWithM
                        ( \n (Typed expr exprT) -> do
                            exprCT <- lift $ typeToC exprT tVars
                            let exprDec = CSVarDec exprCT n
                            exprB <- assignToC (whenVoidPtr exprCT n) expr refs
                            return $ exprDec : exprB
                        )
                        args'
                        args
            return $ argsB ++ [assignF $ CEFunction ('_' : name) $ map (CEVar . CIden) args']
        EVar' name -> do
            (name', _, _) <- lift $ name `findVarRefs` refs
            return [assignF $ CEVar $ CIden name']
        EBlock' block -> blockToC (pure assignF) block refs
        EInfix' (Typed lhs lhsT) infx (Typed rhs rhsT) -> do
            let infx' = infixToCInfix infx
            lhsCT <- lift $ typeToC lhsT tVars
            let lhsN = "__rhs"
            let lhsDec = CSVarDec lhsCT lhsN
            lhs' <- assignToC (CSAssign $ CIden lhsN) lhs refs
            rhsCT <- lift $ typeToC rhsT tVars
            let rhsN = "__rhs"
            let rhsDec = CSVarDec rhsCT rhsN
            rhs' <- assignToC (CSAssign $ CIden rhsN) rhs refs
            return $
                (lhsDec : lhs')
                    ++ (rhsDec : rhs')
                    ++ [ assignF $
                            CEInfix
                                (CEVar $ CIden lhsN)
                                infx'
                                (CEVar $ CIden rhsN)
                       ]

whenVoidPtr :: CType -> String -> CExpression -> CStatement
whenVoidPtr t n =
    if isVoidPtr t
        then CSAssign $ CIden n
        else CSAssignDeref $ CIden n
