module TypeChecker where

import AbsGrammar
import Control.Applicative ((<|>))
import Control.Exception (assert, throw)
import Control.Monad (filterM, foldM, unless, when)
import Control.Monad.Except
  ( ExceptT,
    MonadError (catchError, throwError),
    MonadIO (liftIO),
    MonadTrans (lift),
    foldM_,
    runExceptT,
    withExceptT,
  )
import Control.Monad.Reader (MonadReader (ask, local), ReaderT, runReaderT)
import Control.Monad.State (MonadState (get, put), StateT, modify, runStateT)
import Control.Monad.State.Lazy (gets)
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Utils

data TypeCheckerError
  = UnknownIdentifier Ident BNFC'Position
  | TypeMismatch TypeNoPos TypeNoPos BNFC'Position
  | OperandTypeMismatch String TypeNoPos TypeNoPos [TypeNoPos] BNFC'Position
  | ListElemTypeMismatch TypeCheckerError BNFC'Position
  | NonIndexableType TypeNoPos BNFC'Position
  | IndexIsNotInt TypeNoPos BNFC'Position
  | ListRangeIsNotInt TypeNoPos TypeNoPos BNFC'Position
  | FunctionReturnTypeMismatch TypeNoPos TypeNoPos BNFC'Position
  | ReturnStatementWithConflictingTypes TypeNoPos TypeNoPos BNFC'Position
  | PotentiallyMissingReturn BNFC'Position
  | TopLevelReturn BNFC'Position
  | FunctionTypeVarNotInitialized BNFC'Position
  | AssigningToConst BNFC'Position
  | NotAFunction Ident BNFC'Position
  | InvalidArgumentCount Int Int BNFC'Position
  | InvalidArgumentsType [TypeNoPos] [TypeNoPos] BNFC'Position
  | InvalidPrintArgument TypeNoPos BNFC'Position
  | InvalidVoidUse BNFC'Position
  | CannotInferType TypeNoPos BNFC'Position
  | InvalidState BNFC'Position

type TypeStore = Store TypeWithConstness

type TConfig a = ReaderT Env (StateT TypeStore (ExceptT TypeCheckerError IO)) a

type TypeCheckStmt = (Env, ReturnType)

declareVar :: Ident -> TypeWithConstness -> TConfig Env
declareVar ident typ = do
  env <- ask
  state <- get
  let (loc, newStore) = alloc state
  let newEnv = Map.insert ident loc env
  put $ Store (Map.insert loc typ (store newStore)) (nextLoc newStore)
  return newEnv

getFromStore :: Ident -> BNFC'Position -> TConfig TypeWithConstness
getFromStore ident pos = do
  env <- ask
  store <- gets store
  case Map.lookup ident env of
    Just loc -> case Map.lookup loc store of
      Just typ -> return typ
      Nothing -> throwError $ InvalidState pos
    Nothing -> throwError $ UnknownIdentifier ident pos

getMoreSpecificType :: TypeNoPos -> TypeNoPos -> TConfig TypeNoPos
getMoreSpecificType (TUnknown _) t = return t
getMoreSpecificType t (TUnknown _) = return t
getMoreSpecificType (TList _ t1) (TList _ t2) = do
  t <- getMoreSpecificType t1 t2
  return $ TList () t
getMoreSpecificType t1 _ = return t1

assertCompatibleType :: TypeNoPos -> TypeNoPos -> BNFC'Position -> TConfig ()
assertCompatibleType (TFunction _ args1 ret1) (TFunction _ args2 ret2) pos = do
  assertComaptibleTypes args1 args2 pos
  unless (areTypesCompatible ret1 ret2) $ throwError $ FunctionReturnTypeMismatch ret1 ret2 pos
assertCompatibleType t1 t2 pos = unless (areTypesCompatible t1 t2) $ throwError $ TypeMismatch t1 t2 pos

assertComaptibleTypes :: [TypeNoPos] -> [TypeNoPos] -> BNFC'Position -> TConfig ()
assertComaptibleTypes typs1 typs2 pos = do
  unless (length typs1 == length typs2) $ throwError $ InvalidArgumentCount (length typs1) (length typs2) pos
  mapM_ (\(t1, t2) -> assertCompatibleType t1 t2 pos) (zip typs1 typs2)
    `catchError` \e -> throwError $ InvalidArgumentsType typs1 typs2 pos

assertAllCompatible :: [TypeNoPos] -> TConfig ()
assertAllCompatible [] = return ()
assertAllCompatible types = do
  foldM_ (\t1 t2 -> assertCompatibleType t1 t2 BNFC'NoPosition >> getMoreSpecificType t1 t2) (head types) (tail types)

assertBothOperandsAreOneOf :: TypeNoPos -> TypeNoPos -> [TypeNoPos] -> String -> BNFC'Position -> TConfig ()
assertBothOperandsAreOneOf t1 t2 types oper pos = do
  unless (areTypesEqual t1 t2) $ throwError $ OperandTypeMismatch oper t1 t2 types pos
  if any (areTypesEqual t1) types then return () else throwError $ OperandTypeMismatch oper t1 t2 types pos

typeCheckFunctionReturnValue :: [Arg] -> Block -> TConfig ReturnType
typeCheckFunctionReturnValue [] block = do
  typeCheckBlock block
typeCheckFunctionReturnValue ((MutableArg _ ident typ) : tail) block = do
  newEnv <- declareVar ident (Mutable (mapTypeToTypeNoPos typ))
  local (const newEnv) $ typeCheckFunctionReturnValue tail block
typeCheckFunctionReturnValue ((ConstArg _ ident typ) : tail) block = do
  newEnv <- declareVar ident (Const (mapTypeToTypeNoPos typ))
  local (const newEnv) $ typeCheckFunctionReturnValue tail block

mapFunctionReturnType :: BNFC'Position -> ReturnType -> TConfig TypeNoPos
mapFunctionReturnType _ (CertainReturn t) = return t
mapFunctionReturnType pos (PotentialReturn t) = do
  case t of
    TVoid _ -> return $ TVoid ()
    _ -> do throwError $ PotentiallyMissingReturn pos
mapFunctionReturnType _ NoExplicitReturn = return $ TVoid ()

typeCheckLit :: Literal -> TConfig TypeNoPos
typeCheckLit (LitInt _ _) = return $ TInt ()
typeCheckLit (LitBoolTrue _) = return $ TBool ()
typeCheckLit (LitBoolFalse _) = return $ TBool ()
typeCheckLit (LitString _ _) = return $ TString ()
typeCheckLit (LitList pos content) = do
  case content of
    [] -> return $ TList () (TUnknown ())
    _ -> do
      contentTypes <- mapM typeCheckLit content
      assertAllCompatible contentTypes `catchError` \e -> throwError $ ListElemTypeMismatch e pos
      mostSpecificType <- foldM getMoreSpecificType (head contentTypes) (tail contentTypes)
      return $ TList () mostSpecificType
typeCheckLit (LitFunction pos args ret block) = do
  let argTypes = map (extractType . mapArgToTypeWithConstness) args
  when (any (\x -> hasVoidAtInvalidPosition x False) argTypes) $ throwError $ InvalidVoidUse pos
  let retType = mapTypeToTypeNoPos ret
  when (hasVoidAtInvalidPosition retType True) $ throwError $ InvalidVoidUse pos
  blockRetType <- typeCheckFunctionReturnValue args block >>= mapFunctionReturnType pos
  unless (areTypesCompatible retType blockRetType) $ throwError $ FunctionReturnTypeMismatch retType blockRetType pos
  return $ TFunction () argTypes retType

typeCheckBuiltinFunction :: Ident -> [TypeNoPos] -> BNFC'Position -> TConfig TypeNoPos
typeCheckBuiltinFunction (Ident "print") args pos = do
  unless (length args == 1) $ throwError $ InvalidArgumentCount 1 (length args) pos
  let arg = head args
  let validTypes = [TInt (), TString (), TBool (), TList () (TAny ())]
  unless (isFunctionType arg || any (areTypesCompatible arg) validTypes) $ throwError $ InvalidPrintArgument arg pos
  return $ TVoid ()
typeCheckBuiltinFunction (Ident "len") args pos = do
  unless (length args == 1) $ throwError $ InvalidArgumentCount 1 (length args) pos
  let arg = head args
  unless (isListType arg) $ throwError $ InvalidArgumentsType [TList () (TAny ())] [arg] pos
  return $ TInt ()
typeCheckBuiltinFunction _ _ pos = throwError $ InvalidState pos

typeCheckExpr :: Exp -> TConfig TypeNoPos
typeCheckExpr (ExpVar pos ident) = do
  typ <- getFromStore ident pos
  return $ extractType typ
typeCheckExpr (ExpLit pos lit) = do
  typeCheckLit lit
typeCheckExpr (ExpAddOp pos e1 op e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  case op of
    OpPlus _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString (), TList () (TAny ())] "+" pos
    OpMinus _ -> assertBothOperandsAreOneOf t1 t2 [TInt ()] "-" pos
  return t1
typeCheckExpr (ExpMulOp pos e1 op e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  case op of
    OpTimes _ -> assertBothOperandsAreOneOf t1 t2 [TInt ()] "*" pos
    OpDiv _ -> assertBothOperandsAreOneOf t1 t2 [TInt ()] "/" pos
    OpMod _ -> assertBothOperandsAreOneOf t1 t2 [TInt ()] "%" pos
  return t1
typeCheckExpr (ExpRelOp pos e1 op e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  case op of
    OpLt _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString ()] "<" pos
    OpLe _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString ()] "<=" pos
    OpGt _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString ()] ">" pos
    OpGe _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString ()] ">=" pos
    OpEq _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString (), TBool (), TList () (TAny ())] "==" pos
    OpNeq _ -> assertBothOperandsAreOneOf t1 t2 [TInt (), TString (), TBool (), TList () (TAny ())] "!=" pos
  return $ TBool ()
typeCheckExpr (ExpAnd pos e1 e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  assertBothOperandsAreOneOf t1 t2 [TBool ()] "&&" pos
  return $ TBool ()
typeCheckExpr (ExpOr pos e1 e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  assertBothOperandsAreOneOf t1 t2 [TBool ()] "||" pos
  return $ TBool ()
typeCheckExpr (ExpPrefixOp pos op e) = do
  t <- typeCheckExpr e
  case op of
    OpNeg _ -> assertCompatibleType (TInt ()) t pos
    OpNot _ -> assertCompatibleType (TBool ()) t pos
  return t
typeCheckExpr (ExpIndex pos e1 e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  unless (isIndexableType t1) $ throwError $ NonIndexableType t1 pos
  unless (areTypesEqual t2 (TInt ())) $ throwError $ IndexIsNotInt t2 pos
  return $ extractIndexableType t1
typeCheckExpr (ExpListRange pos e1 e2) = do
  t1 <- typeCheckExpr e1
  t2 <- typeCheckExpr e2
  unless (areTypesEqual t1 (TInt ()) && areTypesEqual t2 (TInt ())) $ throwError $ ListRangeIsNotInt t1 t2 pos
  return $ TList () (TInt ())
typeCheckExpr (ExpApp pos ident exprs) = do
  exprTypes <- mapM typeCheckExpr exprs
  env <- ask
  case Map.lookup ident env of
    Nothing ->
      if isBuiltinFunction ident
        then typeCheckBuiltinFunction ident exprTypes pos
        else throwError $ UnknownIdentifier ident pos
    Just _ -> do
      funType <- getFromStore ident pos
      let (TFunction _ argTypes retType) = extractType funType
      unless (isFunctionType (extractType funType)) $ throwError $ NotAFunction ident pos
      assertComaptibleTypes argTypes exprTypes pos
      return retType

typeCheckElif :: Elif -> TConfig ReturnType
typeCheckElif (Elif pos cond block) = do
  condType <- typeCheckExpr cond
  assertCompatibleType (TBool ()) condType pos
  typeCheckBlock block

-- Returns return type of the block
typeCheckBlock :: Block -> TConfig ReturnType
typeCheckBlock b@(Block _ stmts) = do
  (_, retType) <- typeCheckStmts stmts NoExplicitReturn
  return retType

typeCheckStmtDeclWithInit :: Ident -> TypeWithConstness -> Exp -> BNFC'Position -> TConfig TypeCheckStmt
typeCheckStmtDeclWithInit ident typ expr pos = do
  exprType <- typeCheckExpr expr
  let declType = extractType typ
  when (hasVoidAtInvalidPosition declType False) $ throwError $ InvalidVoidUse pos
  assertCompatibleType declType exprType pos
  env <- declareVar ident typ
  return (env, NoExplicitReturn)

typeCheckStmtAutoDecl :: Ident -> Exp -> Bool -> BNFC'Position -> TConfig TypeCheckStmt
typeCheckStmtAutoDecl ident expr isConst pos = do
  exprType <- typeCheckExpr expr
  when (hasVoidAtInvalidPosition exprType False) $ throwError $ InvalidVoidUse pos
  when (isNotFullyInferred exprType) $ throwError $ CannotInferType exprType pos
  let typ = if isConst then Const exprType else Mutable exprType
  env <- declareVar ident typ
  return (env, NoExplicitReturn)

typeCheckStmt :: Stmt -> TConfig TypeCheckStmt
typeCheckStmt (StmtEmpty pos) = do
  env <- ask
  return (env, NoExplicitReturn)
typeCheckStmt (StmtBlock pos block) = do
  returnType <- typeCheckBlock block
  env <- ask
  return (env, returnType)
typeCheckStmt (StmtExp pos expr) = do
  typeCheckExpr expr
  env <- ask
  return (env, NoExplicitReturn)
typeCheckStmt (StmtReturn pos expr) = do
  retType <- typeCheckExpr expr
  env <- ask
  return (env, CertainReturn retType)
typeCheckStmt (StmtVoidReturn pos) = do
  env <- ask
  return (env, CertainReturn $ TVoid ())
typeCheckStmt (StmtDeclNoInit pos ident typ) = do
  let t = mapTypeToTypeNoPos typ
  when (hasVoidAtInvalidPosition t False) $ throwError $ InvalidVoidUse pos
  when (isFunctionType t) $ throwError $ FunctionTypeVarNotInitialized pos
  env <- declareVar ident (Mutable t)
  return (env, NoExplicitReturn)
typeCheckStmt (StmtDeclInit pos ident typ expr) = do
  typeCheckStmtDeclWithInit ident (Mutable (mapTypeToTypeNoPos typ)) expr pos
typeCheckStmt (StmtConstDecl pos ident typ expr) = do
  typeCheckStmtDeclWithInit ident (Const (mapTypeToTypeNoPos typ)) expr pos
typeCheckStmt (StmtAutoDecl pos ident expr) = do
  typeCheckStmtAutoDecl ident expr False pos
typeCheckStmt (StmtConstAutoDecl pos ident expr) = do
  typeCheckStmtAutoDecl ident expr True pos
typeCheckStmt (StmtAssign pos ident expr) = do
  exprType <- typeCheckExpr expr
  varType <- getFromStore ident pos
  when (isConst varType) $ throwError $ AssigningToConst pos
  assertCompatibleType (extractType varType) exprType pos
  env <- ask
  return (env, NoExplicitReturn)
typeCheckStmt (StmtWhile pos cond block) = do
  condType <- typeCheckExpr cond
  assertCompatibleType (TBool ()) condType pos
  retType <- typeCheckBlock block
  env <- ask
  return (env, weakenReturnType retType)
typeCheckStmt (StmtIfElse pos cond block elifs els) = do
  condType <- typeCheckExpr cond
  assertCompatibleType (TBool ()) condType pos
  blockRetType <- typeCheckBlock block
  elifsRetType <- mapM typeCheckElif elifs
  elsRetType <- typeCheckBlock els
  mergedType <- foldM (\ret x -> mergeReturnTypes ret x pos) blockRetType (elifsRetType ++ [elsRetType])
  env <- ask
  return (env, mergedType)
  where
    mergeReturnTypes :: ReturnType -> ReturnType -> BNFC'Position -> TConfig ReturnType
    mergeReturnTypes NoExplicitReturn t _ = return $ weakenReturnType t
    mergeReturnTypes t NoExplicitReturn _ = return $ weakenReturnType t
    mergeReturnTypes t1 t2 pos = do
      let t1' = extractReturnType t1
      let t2' = extractReturnType t2
      assertCompatibleType t1' t2' pos `catchError` \e -> throwError $ ReturnStatementWithConflictingTypes t1' t2' pos
      let potentialReturn = isPotentialReturn t1 || isPotentialReturn t2
      let certainReturn = isCertainReturn t1 || isCertainReturn t2
      return $ if potentialReturn then PotentialReturn t1' else if certainReturn then CertainReturn t1' else NoExplicitReturn
typeCheckStmt (StmtIf pos cond block elifs) = do
  typeCheckStmt (StmtIfElse pos cond block elifs (Block pos []))
typeCheckStmt (StmtFunDef pos ident args ret block) = do
  let argTypes = map (extractType . mapArgToTypeWithConstness) args
  when (any (\x -> hasVoidAtInvalidPosition x False) argTypes) $ throwError $ InvalidVoidUse pos
  let retType = mapTypeToTypeNoPos ret
  when (hasVoidAtInvalidPosition retType True) $ throwError $ InvalidVoidUse pos
  let funType = TFunction () argTypes retType
  env <- declareVar ident (Const funType)
  local (const env) $ typeCheckFunctionReturnValue args block
  blockRetType <- local (const env) $ typeCheckFunctionReturnValue args block
  mappedBlockRetType <- mapFunctionReturnType pos blockRetType
  unless (areTypesCompatible retType mappedBlockRetType) $ throwError $ FunctionReturnTypeMismatch retType mappedBlockRetType pos
  return (env, NoExplicitReturn)
typeCheckStmt (StmtFor pos ident expr block) = do
  exprType <- typeCheckExpr expr
  unless (isListType exprType) $ throwError $ NonIndexableType exprType pos
  when (isNotFullyInferred exprType) $ throwError $ CannotInferType exprType pos
  let listType = extractIndexableType exprType
  newEnv <- declareVar ident (Const listType)
  blockRetType <- local (const newEnv) $ typeCheckBlock block
  env <- ask
  return (env, weakenReturnType blockRetType) -- weaken return type because it's not guaranteed that the block will ever be executed

typeCheckStmts :: [Stmt] -> ReturnType -> TConfig TypeCheckStmt
typeCheckStmts [] prevRetType = do
  env <- ask
  return (env, prevRetType)
typeCheckStmts (stmt : tail) prevRetType = do
  (env, ret) <- typeCheckStmt stmt
  mergedType <- mergeReturnTypes prevRetType ret (hasPosition stmt)
  local (const env) $ typeCheckStmts tail mergedType
  where
    mergeReturnTypes :: ReturnType -> ReturnType -> BNFC'Position -> TConfig ReturnType
    mergeReturnTypes NoExplicitReturn t _ = return t
    mergeReturnTypes t NoExplicitReturn _ = return t
    mergeReturnTypes t1 t2 pos = do
      let t1' = extractReturnType t1
      let t2' = extractReturnType t2
      assertCompatibleType t1' t2' pos `catchError` \e -> throwError $ ReturnStatementWithConflictingTypes t1' t2' pos
      let potentialReturn = isPotentialReturn t1 || isPotentialReturn t2
      let certainReturn = isCertainReturn t1 || isCertainReturn t2
      return $ if certainReturn then CertainReturn t1' else if potentialReturn then PotentialReturn t1' else NoExplicitReturn

typeCheckProgram :: Program -> TConfig ()
typeCheckProgram (Program pos stms) = do
  (_, retType) <- typeCheckStmts stms NoExplicitReturn
  unless (noExplicitReturn retType) $ throwError $ TopLevelReturn pos

showError :: TypeCheckerError -> String
showError e = "Type checker error:\n" ++ show e

runTypeChecker :: Program -> ExceptT String IO ()
runTypeChecker program = do
  let env = Map.empty
  let store = Store Map.empty 0
  withExceptT showError $ runStateT (runReaderT (typeCheckProgram program) env) store
  return ()

instance Show TypeCheckerError where
  show (InvalidState pos) = showPosition pos ++ "Invalid type checker state"
  show (UnknownIdentifier (Ident id) pos) = showPosition pos ++ "Unknown identifier: " ++ id
  show (TypeMismatch expected actual pos) = showPosition pos ++ "Type mismatch: expected to be " ++ show expected ++ ", got " ++ show actual
  show (OperandTypeMismatch op t1 t2 types pos) = showPosition pos ++ "Type mismatch in " ++ op ++ " operator: expected both operands to be one of " ++ show types ++ ", got " ++ show t1 ++ " and " ++ show t2
  show (ListElemTypeMismatch e pos) = showPosition pos ++ "List element type mismatch caused by:\n" ++ show e
  show (NonIndexableType t pos) = showPosition pos ++ "Non-indexable type: " ++ show t
  show (IndexIsNotInt t pos) = showPosition pos ++ "Index is not of type int, got " ++ show t
  show (ListRangeIsNotInt t1 t2 pos) = showPosition pos ++ "List range is not of type int, got " ++ show t1 ++ " and " ++ show t2
  show (FunctionReturnTypeMismatch expected actual pos) = showPosition pos ++ "Function return type mismatch: expected " ++ show expected ++ ", got " ++ show actual
  show (FunctionTypeVarNotInitialized pos) = showPosition pos ++ "Function type variable not initialized"
  show (ReturnStatementWithConflictingTypes t1 t2 pos) = showPosition pos ++ "Return statement with conflicting types: " ++ show t1 ++ " and " ++ show t2
  show (PotentiallyMissingReturn pos) = showPosition pos ++ "Cannot guarantee that function returns a value. Consider adding a return statement at the end of the function."
  show (AssigningToConst pos) = showPosition pos ++ "Assigning to const variable"
  show (NotAFunction (Ident id) pos) = showPosition pos ++ id ++ " is not a function"
  show (InvalidArgumentCount expected actual pos) = showPosition pos ++ "Invalid argument count: expected " ++ show expected ++ ", got " ++ show actual
  show (InvalidArgumentsType expected actual pos) = showPosition pos ++ "Invalid argument types: expected " ++ show expected ++ ", got " ++ show actual
  show (InvalidVoidUse pos) = showPosition pos ++ "Void can only be used as a direct return type"
  show (InvalidPrintArgument t pos) = showPosition pos ++ "Invalid argument type for print function: expected one of [int, string, bool, any[]], got " ++ show t
  show (CannotInferType t pos) = showPosition pos ++ "Cannot fully infer type, got: " ++ show t
  show (TopLevelReturn pos) = showPosition pos ++ "Return statement at top level"
