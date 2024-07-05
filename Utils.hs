module Utils where

import AbsGrammar
import Types

showPosition :: BNFC'Position -> String
showPosition pos =
  case pos of
    Just (line, column) -> "Line: " ++ show line ++ ", Column: " ++ show column ++ " -> "
    Nothing -> ""

alloc :: Store a -> (Loc, Store a)
alloc s = (nextLoc s, Store (store s) (nextLoc s + 1))

mapTypeToTypeNoPos :: Type -> TypeNoPos
mapTypeToTypeNoPos (TInt _) = TInt ()
mapTypeToTypeNoPos (TBool _) = TBool ()
mapTypeToTypeNoPos (TVoid _) = TVoid ()
mapTypeToTypeNoPos (TString _) = TString ()
mapTypeToTypeNoPos (TFunction _ args ret) = TFunction () (map mapTypeToTypeNoPos args) (mapTypeToTypeNoPos ret)
mapTypeToTypeNoPos (TList _ t) = TList () (mapTypeToTypeNoPos t)
mapTypeToTypeNoPos (TUnknown _) = TUnknown ()

mapArgToTypeWithConstness :: Arg -> TypeWithConstness
mapArgToTypeWithConstness (MutableArg _ _ t) = Mutable (mapTypeToTypeNoPos t)
mapArgToTypeWithConstness (ConstArg _ _ t) = Const (mapTypeToTypeNoPos t)

filterMaybe :: [Maybe a] -> [a]
filterMaybe [] = []
filterMaybe (Just x : xs) = x : filterMaybe xs

extractType :: TypeWithConstness -> TypeNoPos
extractType (Const t) = t
extractType (Mutable t) = t

extractIdent :: Arg -> Ident
extractIdent (MutableArg _ ident _) = ident
extractIdent (ConstArg _ ident _) = ident

hasVoidAtInvalidPosition :: TypeNoPos -> Bool -> Bool
hasVoidAtInvalidPosition (TVoid {}) isReturnType = not isReturnType
hasVoidAtInvalidPosition (TList _ t) _ = hasVoidAtInvalidPosition t False
hasVoidAtInvalidPosition (TFunction _ args ret) _ =
  any (\x -> hasVoidAtInvalidPosition x False) args || case ret of
    TVoid {} -> False
    t -> hasVoidAtInvalidPosition t False
hasVoidAtInvalidPosition _ _ = False

isNotFullyInferred :: TypeNoPos -> Bool
isNotFullyInferred (TUnknown _) = True
isNotFullyInferred (TList _ t) = isNotFullyInferred t
isNotFullyInferred _ = False

isFunctionType :: TypeNoPos -> Bool
isFunctionType (TFunction {}) = True
isFunctionType _ = False

isListType :: TypeNoPos -> Bool
isListType (TList {}) = True
isListType _ = False

isConst :: TypeWithConstness -> Bool
isConst (Const _) = True
isConst _ = False

noExplicitReturn :: ReturnType -> Bool
noExplicitReturn NoExplicitReturn = True
noExplicitReturn _ = False

isCertainReturn :: ReturnType -> Bool
isCertainReturn (CertainReturn _) = True
isCertainReturn _ = False

isPotentialReturn :: ReturnType -> Bool
isPotentialReturn (PotentialReturn _) = True
isPotentialReturn _ = False

extractReturnType :: ReturnType -> TypeNoPos
extractReturnType (PotentialReturn t) = t
extractReturnType (CertainReturn t) = t
extractReturnType NoExplicitReturn = error "NoExplicitReturn has no return type"

weakenReturnType :: ReturnType -> ReturnType
weakenReturnType (CertainReturn t) = PotentialReturn t
weakenReturnType t = t

isIndexableType :: TypeNoPos -> Bool
isIndexableType (TList {}) = True
isIndexableType (TString {}) = True
isIndexableType _ = False

extractIndexableType :: TypeNoPos -> TypeNoPos
extractIndexableType (TList _ t) = t
extractIndexableType (TString _) = TString ()

isBuiltinFunction :: Ident -> Bool
isBuiltinFunction (Ident "print") = True
isBuiltinFunction (Ident "len") = True
isBuiltinFunction _ = False

areTypesEqual :: TypeNoPos -> TypeNoPos -> Bool
areTypesEqual (TUnknown _) _ = False
areTypesEqual _ (TUnknown _) = False
areTypesEqual (TAny _) _ = True
areTypesEqual _ (TAny _) = True
areTypesEqual (TList _ t1) (TList _ t2) = areTypesEqual t1 t2
areTypesEqual t1 t2 = t1 == t2

areTypesCompatible :: TypeNoPos -> TypeNoPos -> Bool
areTypesCompatible (TUnknown _) _ = True
areTypesCompatible _ (TUnknown _) = True
areTypesCompatible (TAny _) _ = True
areTypesCompatible _ (TAny _) = True
areTypesCompatible (TList _ t1) (TList _ t2) = areTypesCompatible t1 t2
areTypesCompatible (TFunction _ args1 ret1) (TFunction _ args2 ret2) =
  areTypesCompatible ret1 ret2 && length args1 == length args2 && all (uncurry areTypesCompatible) (zip args1 args2)
areTypesCompatible t1 t2 = t1 == t2

getRelOpFunc :: (Ord a) => RelOp -> a -> a -> Bool
getRelOpFunc (OpLt _) = (<)
getRelOpFunc (OpLe _) = (<=)
getRelOpFunc (OpGt _) = (>)
getRelOpFunc (OpGe _) = (>=)
getRelOpFunc (OpEq _) = (==)
getRelOpFunc (OpNeq _) = (/=)