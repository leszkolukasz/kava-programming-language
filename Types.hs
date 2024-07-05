{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import AbsGrammar
import Control.Applicative (Alternative (empty, (<|>)))
import Data.Map (Map)

type TypeNoPos = Type' ()

data TypeWithConstness = Const TypeNoPos | Mutable TypeNoPos

type Loc = Int

type Env = Map Ident Loc

data Store a = Store {store :: Map Loc a, nextLoc :: Loc}

-- Potential return is used with `if` and `while` statements
-- If `if` statement is of type `if, elif, else` and all branches return, then the return type is CertainReturn
data ReturnType
  = NoExplicitReturn
  | PotentialReturn TypeNoPos
  | CertainReturn TypeNoPos

data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VList [Value]
  | VFunction (Maybe Ident) Env [Ident] Block -- Name (Nothing if anon) | Env | Arg names | Block
  | VVoid
  deriving (Eq)

instance {-# OVERLAPPING #-} Show [TypeNoPos] where
  show [] = ""
  show l = foldr (\s -> (showString (show s ++ ", ") .)) id (init l) "" ++ show (last l)

instance {-# OVERLAPPING #-} Show TypeNoPos where
  show (TInt _) = "int"
  show (TBool _) = "bool"
  show (TVoid _) = "void"
  show (TString _) = "string"
  show (TFunction _ args ret) = "(" ++ show args ++ ") -> " ++ show ret
  show (TList _ t) = show t ++ "[]"
  show (TUnknown _) = "unknown"
  show (TAny _) = "any"

instance Show TypeWithConstness where
  show (Const t) = "const " ++ show t
  show (Mutable t) = show t

instance Show Value where
  show (VInt v) = show v
  show (VBool v) = show v
  show (VString v) = show v
  show (VList v) = show v
  show (VFunction name args _ isAnon) = case name of
    Just (Ident n) -> "<func " ++ n ++ ">"
    Nothing -> "<anon func>"