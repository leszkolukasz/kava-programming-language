-- File generated by the BNF Converter (bnfc 2.9.5).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for PrintGrammar.

module PrintGrammar where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified AbsGrammar

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsGrammar.Ident where
  prt _ (AbsGrammar.Ident i) = doc $ showString i
instance Print (AbsGrammar.Program' a) where
  prt i = \case
    AbsGrammar.Program _ stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print (AbsGrammar.Block' a) where
  prt i = \case
    AbsGrammar.Block _ stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print (AbsGrammar.Arg' a) where
  prt i = \case
    AbsGrammar.MutableArg _ id_ type_ -> prPrec i 0 (concatD [prt 0 id_, doc (showString ":"), prt 0 type_])
    AbsGrammar.ConstArg _ id_ type_ -> prPrec i 0 (concatD [doc (showString "const"), prt 0 id_, doc (showString ":"), prt 0 type_])

instance Print (AbsGrammar.Elif' a) where
  prt i = \case
    AbsGrammar.Elif _ exp block -> prPrec i 0 (concatD [doc (showString "elif"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 block])

instance Print (AbsGrammar.Stmt' a) where
  prt i = \case
    AbsGrammar.StmtEmpty _ -> prPrec i 0 (concatD [doc (showString ";")])
    AbsGrammar.StmtBlock _ block -> prPrec i 0 (concatD [prt 0 block])
    AbsGrammar.StmtDeclNoInit _ id_ type_ -> prPrec i 0 (concatD [doc (showString "let"), prt 0 id_, doc (showString ":"), prt 0 type_, doc (showString ";")])
    AbsGrammar.StmtDeclInit _ id_ type_ exp -> prPrec i 0 (concatD [doc (showString "let"), prt 0 id_, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtConstDecl _ id_ type_ exp -> prPrec i 0 (concatD [doc (showString "const"), prt 0 id_, doc (showString ":"), prt 0 type_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtAutoDecl _ id_ exp -> prPrec i 0 (concatD [doc (showString "auto"), prt 0 id_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtConstAutoDecl _ id_ exp -> prPrec i 0 (concatD [doc (showString "const"), doc (showString "auto"), prt 0 id_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtAssign _ id_ exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtReturn _ exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtVoidReturn _ -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsGrammar.StmtIf _ exp block elifs -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 block, prt 0 elifs])
    AbsGrammar.StmtIfElse _ exp block1 elifs block2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 block1, prt 0 elifs, doc (showString "else"), prt 0 block2])
    AbsGrammar.StmtWhile _ exp block -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 block])
    AbsGrammar.StmtFor _ id_ exp block -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), doc (showString "const"), prt 0 id_, doc (showString "of"), prt 0 exp, doc (showString ")"), prt 0 block])
    AbsGrammar.StmtExp _ exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    AbsGrammar.StmtFunDef _ id_ args type_ block -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id_, doc (showString "("), prt 0 args, doc (showString "):"), prt 0 type_, prt 0 block])

instance Print [AbsGrammar.Stmt' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsGrammar.Arg' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsGrammar.Elif' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (AbsGrammar.Type' a) where
  prt i = \case
    AbsGrammar.TInt _ -> prPrec i 0 (concatD [doc (showString "int")])
    AbsGrammar.TBool _ -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsGrammar.TVoid _ -> prPrec i 0 (concatD [doc (showString "void")])
    AbsGrammar.TString _ -> prPrec i 0 (concatD [doc (showString "string")])
    AbsGrammar.TFunction _ types type_ -> prPrec i 0 (concatD [doc (showString "F("), prt 0 types, doc (showString ")"), doc (showString "->"), prt 0 type_])
    AbsGrammar.TList _ type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "[]")])
    AbsGrammar.TUnknown _ -> prPrec i 0 (concatD [doc (showString "unknown")])
    AbsGrammar.TAny _ -> prPrec i 0 (concatD [doc (showString "any")])

instance Print [AbsGrammar.Type' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Literal' a) where
  prt i = \case
    AbsGrammar.LitInt _ n -> prPrec i 0 (concatD [prt 0 n])
    AbsGrammar.LitBoolTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    AbsGrammar.LitBoolFalse _ -> prPrec i 0 (concatD [doc (showString "false")])
    AbsGrammar.LitString _ str -> prPrec i 0 (concatD [printString str])
    AbsGrammar.LitList _ literals -> prPrec i 0 (concatD [doc (showString "List.of("), prt 0 literals, doc (showString ")")])
    AbsGrammar.LitFunction _ args type_ block -> prPrec i 0 (concatD [doc (showString "("), prt 0 args, doc (showString "):"), prt 0 type_, doc (showString "->"), prt 0 block])

instance Print [AbsGrammar.Literal' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.Exp' a) where
  prt i = \case
    AbsGrammar.ExpVar _ id_ -> prPrec i 9 (concatD [prt 0 id_])
    AbsGrammar.ExpLit _ literal -> prPrec i 9 (concatD [prt 0 literal])
    AbsGrammar.ExpApp _ id_ exps -> prPrec i 8 (concatD [prt 0 id_, doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsGrammar.ExpIndex _ exp1 exp2 -> prPrec i 7 (concatD [prt 8 exp1, doc (showString "["), prt 0 exp2, doc (showString "]")])
    AbsGrammar.ExpListRange _ exp1 exp2 -> prPrec i 6 (concatD [prt 7 exp1, doc (showString ".."), prt 6 exp2])
    AbsGrammar.ExpPrefixOp _ prefixop exp -> prPrec i 5 (concatD [prt 0 prefixop, prt 6 exp])
    AbsGrammar.ExpMulOp _ exp1 mulop exp2 -> prPrec i 4 (concatD [prt 4 exp1, prt 0 mulop, prt 5 exp2])
    AbsGrammar.ExpAddOp _ exp1 addop exp2 -> prPrec i 3 (concatD [prt 3 exp1, prt 0 addop, prt 4 exp2])
    AbsGrammar.ExpRelOp _ exp1 relop exp2 -> prPrec i 2 (concatD [prt 2 exp1, prt 0 relop, prt 3 exp2])
    AbsGrammar.ExpAnd _ exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "&&"), prt 2 exp2])
    AbsGrammar.ExpOr _ exp1 exp2 -> prPrec i 0 (concatD [prt 1 exp1, doc (showString "||"), prt 1 exp2])

instance Print [AbsGrammar.Exp' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (AbsGrammar.PrefixOp' a) where
  prt i = \case
    AbsGrammar.OpNeg _ -> prPrec i 0 (concatD [doc (showString "-")])
    AbsGrammar.OpNot _ -> prPrec i 0 (concatD [doc (showString "!")])

instance Print (AbsGrammar.AddOp' a) where
  prt i = \case
    AbsGrammar.OpPlus _ -> prPrec i 0 (concatD [doc (showString "+")])
    AbsGrammar.OpMinus _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (AbsGrammar.MulOp' a) where
  prt i = \case
    AbsGrammar.OpTimes _ -> prPrec i 0 (concatD [doc (showString "*")])
    AbsGrammar.OpDiv _ -> prPrec i 0 (concatD [doc (showString "/")])
    AbsGrammar.OpMod _ -> prPrec i 0 (concatD [doc (showString "%")])

instance Print (AbsGrammar.RelOp' a) where
  prt i = \case
    AbsGrammar.OpLt _ -> prPrec i 0 (concatD [doc (showString "<")])
    AbsGrammar.OpLe _ -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsGrammar.OpGt _ -> prPrec i 0 (concatD [doc (showString ">")])
    AbsGrammar.OpGe _ -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsGrammar.OpEq _ -> prPrec i 0 (concatD [doc (showString "==")])
    AbsGrammar.OpNeq _ -> prPrec i 0 (concatD [doc (showString "!=")])
