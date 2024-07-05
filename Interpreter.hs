module Interpreter where

import AbsGrammar
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO), withExceptT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT, runReaderT)
import Control.Monad.State (MonadState (get), StateT, runStateT)
import Control.Monad.State.Lazy (MonadState (put), gets)
import qualified Data.Map as Map
import Types
import Utils

data RuntimeError
  = DivisionByZero BNFC'Position
  | ModuloByZero BNFC'Position
  | IndexOutOfBounds BNFC'Position
  | InvalidState BNFC'Position

type ValueStore = Store Value

type IConfig a = ReaderT Env (StateT ValueStore (ExceptT RuntimeError IO)) a

type EvalStmt = (Env, Maybe Value) -- (new env, return value)

declareVar :: Ident -> Value -> IConfig Env
declareVar ident val = do
  env <- ask
  state <- get
  let (loc, newStore) = alloc state
  let newEnv = Map.insert ident loc env
  put $ Store (Map.insert loc val (store newStore)) (nextLoc newStore)
  return newEnv

assignVar :: Ident -> Value -> BNFC'Position -> IConfig ()
assignVar ident val pos = do
  env <- ask
  st <- get
  case Map.lookup ident env of
    Just loc -> put $ Store (Map.insert loc val (store st)) (nextLoc st)
    Nothing -> throwError $ InvalidState pos

getDefaultVal :: Type -> Maybe Value
getDefaultVal (TInt _) = Just $ VInt 0
getDefaultVal (TBool _) = Just $ VBool False
getDefaultVal (TString _) = Just $ VString ""
getDefaultVal (TList _ _) = Just $ VList []
getDefaultVal _ = Nothing

getFromStore :: Ident -> BNFC'Position -> IConfig Value
getFromStore ident pos = do
  env <- ask
  store <- gets store
  case Map.lookup ident env of
    Just loc -> case Map.lookup loc store of
      Just typ -> return typ
      Nothing -> throwError $ InvalidState pos
    Nothing -> throwError $ InvalidState pos

evaluateLiteral :: Literal -> IConfig Value
evaluateLiteral (LitInt _ v) = return $ VInt v
evaluateLiteral (LitBoolTrue _) = return $ VBool True
evaluateLiteral (LitBoolFalse _) = return $ VBool False
evaluateLiteral (LitString _ v) = return $ VString v
evaluateLiteral (LitList _ lits) = do
  vals <- mapM evaluateLiteral lits
  return $ VList vals
evaluateLiteral (LitFunction _ args _ block) = do
  env <- ask
  return $ VFunction Nothing env (map extractIdent args) block

evaluateBlock :: Block -> IConfig EvalStmt
evaluateBlock (Block _ stmts) = do
  env <- ask
  (_, ret) <- evaluateStmts stmts
  return (env, ret)

evaluateBuiltinFunction :: Ident -> [Value] -> BNFC'Position -> IConfig Value
evaluateBuiltinFunction (Ident "print") [val] _ = do
  liftIO $ print val
  return VVoid
evaluateBuiltinFunction (Ident "len") [VList l] _ = return $ VInt (toInteger $ length l)
evaluateBuiltinFunction _ _ pos = throwError $ InvalidState pos

evaluateFunction :: Ident -> [Value] -> BNFC'Position -> IConfig Value
evaluateFunction ident vals pos = do
  VFunction name env args block <- getFromStore ident pos
  newEnv <- case name of
    Just n -> local (const env) $ declareVar n (VFunction (Just n) env args block)
    Nothing -> return env
  local (const newEnv) $ evaluateFunctionHelper args vals block
  where
    evaluateFunctionHelper :: [Ident] -> [Value] -> Block -> IConfig Value
    evaluateFunctionHelper [] [] block = do
      (_, ret) <- evaluateBlock block
      case ret of
        Just val -> return val
        Nothing -> return VVoid
    evaluateFunctionHelper (arg : args) (val : vals) block = do
      env <- declareVar arg val
      local (const env) (evaluateFunctionHelper args vals block)
    evaluateFunctionHelper _ _ _ = throwError $ InvalidState pos

evaluateExpr :: Exp -> IConfig Value
evaluateExpr (ExpLit _ lit) = evaluateLiteral lit
evaluateExpr (ExpVar pos ident) = getFromStore ident pos
evaluateExpr (ExpPrefixOp pos op exp) = do
  val <- evaluateExpr exp
  case op of
    OpNeg _ -> case val of
      VInt v -> return $ VInt (-v)
      _ -> throwError $ InvalidState pos
    OpNot _ -> case val of
      VBool v -> return $ VBool (not v)
      _ -> throwError $ InvalidState pos
evaluateExpr (ExpAddOp pos exp1 op exp2) = do
  val1 <- evaluateExpr exp1
  val2 <- evaluateExpr exp2
  case (val1, val2) of
    (VInt v1, VInt v2) -> case op of
      OpPlus _ -> return $ VInt (v1 + v2)
      OpMinus _ -> return $ VInt (v1 - v2)
    (VString v1, VString v2) -> case op of
      OpPlus _ -> return $ VString (v1 ++ v2)
      _ -> throwError $ InvalidState pos
    (VList v1, VList v2) -> case op of
      OpPlus _ -> return $ VList (v1 ++ v2)
      _ -> throwError $ InvalidState pos
    _ -> throwError $ InvalidState pos
evaluateExpr (ExpMulOp pos exp1 op exp2) = do
  val1 <- evaluateExpr exp1
  val2 <- evaluateExpr exp2
  case (val1, val2) of
    (VInt v1, VInt v2) -> case op of
      OpTimes _ -> return $ VInt (v1 * v2)
      OpDiv _ -> if v2 == 0 then throwError $ DivisionByZero pos else return $ VInt (v1 `div` v2)
      OpMod _ -> if v2 == 0 then throwError $ ModuloByZero pos else return $ VInt (v1 `mod` v2)
    _ -> throwError $ InvalidState pos
evaluateExpr (ExpRelOp pos exp1 op exp2) = do
  val1 <- evaluateExpr exp1
  val2 <- evaluateExpr exp2
  case (val1, val2) of
    (VInt v1, VInt v2) -> return $ VBool $ getRelOpFunc op v1 v2
    (VBool v1, VBool v2) -> return $ VBool $ getRelOpFunc op v1 v2
    (VString v1, VString v2) -> return $ VBool $ getRelOpFunc op v1 v2
    (VList v1, VList v2) -> case op of
      OpEq _ -> return $ VBool $ v1 == v2
      OpNeq _ -> return $ VBool $ v1 /= v2
      _ -> throwError $ InvalidState pos
    _ -> throwError $ InvalidState pos
evaluateExpr (ExpAnd pos exp1 exp2) = do
  val1 <- evaluateExpr exp1
  val2 <- evaluateExpr exp2
  case (val1, val2) of
    (VBool v1, VBool v2) -> return $ VBool $ v1 && v2
    _ -> throwError $ InvalidState pos
evaluateExpr (ExpOr pos exp1 exp2) = do
  val1 <- evaluateExpr exp1
  val2 <- evaluateExpr exp2
  case (val1, val2) of
    (VBool v1, VBool v2) -> return $ VBool $ v1 || v2
    _ -> throwError $ InvalidState pos
evaluateExpr (ExpApp pos ident exps) = do
  vals <- mapM evaluateExpr exps
  env <- ask
  case Map.lookup ident env of
    Just _ -> evaluateFunction ident vals pos
    Nothing ->
      if isBuiltinFunction ident
        then evaluateBuiltinFunction ident vals pos
        else throwError $ InvalidState pos
evaluateExpr (ExpIndex pos exp1 exp2) = do
  v <- evaluateExpr exp1
  VInt idx <- evaluateExpr exp2
  len <- case v of
    VList l -> return $ toInteger $ length l
    VString s -> return $ toInteger $ length s
    _ -> throwError $ InvalidState pos
  if idx < 0 || idx >= len
    then
      if idx + len < 0 || idx + len >= len
        then throwError $ IndexOutOfBounds pos
        else getItem v (idx + len)
    else getItem v idx
  where
    getItem :: Value -> Integer -> IConfig Value
    getItem (VList l) idx = return $ l !! fromInteger idx
    getItem (VString s) idx = return $ VString [s !! fromInteger idx]
    getItem _ _ = throwError $ InvalidState pos
evaluateExpr (ExpListRange pos exp1 exp2) = do
  VInt from <- evaluateExpr exp1
  VInt to <- evaluateExpr exp2
  let l = if from > to then reverse [to .. from] else [from .. to]
  return $ VList $ map VInt l

findFirstTrueElif :: [Elif] -> IConfig (Maybe Block)
findFirstTrueElif [] = return Nothing
findFirstTrueElif ((Elif pos cond block) : elifs) = do
  expr <- evaluateExpr cond
  case expr of
    VBool True -> return $ Just block
    VBool False -> findFirstTrueElif elifs
    _ -> throwError $ InvalidState pos

evaluateStmt :: Stmt -> IConfig EvalStmt
evaluateStmt (StmtEmpty _) = do
  env <- ask
  return (env, Nothing)
evaluateStmt (StmtExp _ exp) = do
  env <- ask
  evaluateExpr exp
  return (env, Nothing)
evaluateStmt (StmtAssign pos ident exp) = do
  env <- ask
  val <- evaluateExpr exp
  assignVar ident val pos
  return (env, Nothing)
evaluateStmt (StmtDeclInit pos ident _ exp) = do
  val <- evaluateExpr exp
  env <- declareVar ident val
  return (env, Nothing)
evaluateStmt (StmtConstDecl pos ident _ exp) = do
  val <- evaluateExpr exp
  env <- declareVar ident val
  return (env, Nothing)
evaluateStmt (StmtAutoDecl pos ident exp) = do
  val <- evaluateExpr exp
  env <- declareVar ident val
  return (env, Nothing)
evaluateStmt (StmtConstAutoDecl pos ident exp) = do
  val <- evaluateExpr exp
  env <- declareVar ident val
  return (env, Nothing)
evaluateStmt (StmtDeclNoInit pos ident typ) = do
  case getDefaultVal typ of
    Nothing -> throwError $ InvalidState pos
    Just v -> do
      env <- declareVar ident v
      return (env, Nothing)
evaluateStmt (StmtBlock _ block) = evaluateBlock block
evaluateStmt (StmtIfElse pos cond block elifs els) = do
  expr <- evaluateExpr cond
  case expr of
    VBool True -> evaluateBlock block
    VBool False -> do
      blck <- findFirstTrueElif elifs
      case blck of
        Just b -> evaluateBlock b
        Nothing -> evaluateBlock els
    _ -> throwError $ InvalidState pos
evaluateStmt (StmtIf pos cond block elifs) =
  evaluateStmt (StmtIfElse pos cond block elifs (Block pos []))
evaluateStmt (StmtWhile pos cond block) = do
  env <- ask
  expr <- evaluateExpr cond
  case expr of
    VBool True -> do
      (_, ret) <- evaluateBlock block
      case ret of
        Just _ -> return (env, ret)
        Nothing -> evaluateStmt (StmtWhile pos cond block)
    VBool False -> return (env, Nothing)
    _ -> throwError $ InvalidState pos
evaluateStmt (StmtReturn pos exp) = do
  env <- ask
  val <- evaluateExpr exp
  return (env, Just val)
evaluateStmt (StmtVoidReturn _) = do
  env <- ask
  return (env, Just VVoid)
evaluateStmt (StmtFunDef pos ident args _ block) = do
  env <- ask
  let fun = VFunction (Just ident) env (map extractIdent args) block
  newEnv <- declareVar ident fun
  return (newEnv, Nothing)
evaluateStmt (StmtFor pos ident exp block) = do
  env <- ask
  VList l <- evaluateExpr exp
  loop l ident env
  where
    loop :: [Value] -> Ident -> Env -> IConfig EvalStmt
    loop [] ident env = return (env, Nothing)
    loop (x : xs) ident env = do
      newEnv <- declareVar ident x
      (_, ret) <- local (const newEnv) $ evaluateBlock block
      case ret of
        Just _ -> return (newEnv, ret)
        Nothing -> loop xs ident env

evaluateStmts :: [Stmt] -> IConfig EvalStmt
evaluateStmts [] = do
  env <- ask
  return (env, Nothing)
evaluateStmts (stmt : stmts) = do
  (env, ret) <- evaluateStmt stmt
  case ret of
    Just _ -> return (env, ret)
    Nothing -> local (const env) (evaluateStmts stmts)

evaluate :: Program -> IConfig ()
evaluate (Program pos stms) = do
  (_, ret) <- evaluateStmts stms
  case ret of
    Just _ -> throwError $ InvalidState pos
    Nothing -> return ()

showError :: RuntimeError -> String
showError e = "Runtime error:\n" ++ show e

runInterpreter :: Program -> ExceptT String IO ()
runInterpreter program = do
  let env = Map.empty
  let store = Store Map.empty 0
  withExceptT showError $ runStateT (runReaderT (evaluate program) env) store
  return ()

instance Show RuntimeError where
  show (DivisionByZero pos) = showPosition pos ++ "Division by zero"
  show (ModuloByZero pos) = showPosition pos ++ "Second argument of modulo cannot be zero"
  show (IndexOutOfBounds pos) = showPosition pos ++ "Index out of bounds"
  show (InvalidState pos) = showPosition pos ++ "Invalid state. Most likely cause by a bug in type cheker."