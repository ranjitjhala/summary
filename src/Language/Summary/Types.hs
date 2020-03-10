{-# LANGUAGE DeriveFunctor #-}

module Language.Summary.Types where 

import Data.Text as T
import Language.Summary.UX

-- | Identifiers --------------------------------------------------------------
type Id    = T.Text

-- | Field names --------------------------------------------------------------
type Field = T.Text 

-- | LValues ------------------------------------------------------------------
data LVal a 
  = LvId    !Id          a                      -- ^ x,y,z
  | LvFld   !Id !Field   a                      -- ^ x.f
  deriving (Eq, Ord, Show, Functor)

-- | Expressions (no updates) -------------------------------------------------
data Expr a 
  = ELv     !(LVal a)                a          -- ^ e.fld
  | ECall   !Id         ![Expr a]    a          -- ^ f(e1,...) 
  | ENew                             a          -- ^ new    
  deriving (Eq, Ord, Show, Functor)

-- | Statements ---------------------------------------------------------------
data Stmt a
  = SAsn    !(LVal a)   !(Expr a)           a   -- ^ lv := e 
  | SSeq    ![(Stmt a)]                     a   -- ^ s1;...;sn 
  | SIf     !(Expr a)   !(Stmt a) !(Stmt a) a   -- ^ if (e) { s1 } else { s2 }
  | SWhl    !(Expr a)   !(Stmt a)           a   -- ^ while (e) { s }
  | SRet    !(Expr a)                       a   -- ^ return e
  deriving (Eq, Ord, Show, Functor)

data Func a = Func 
  { fnName   :: !Id           -- ^ name
  , fnParams :: ![Id]         -- ^ params
  , fnBody   :: !(Stmt a)     -- ^ code
  , fnLabel  :: a             -- ^ metadata
  }
  deriving (Eq, Ord, Show, Functor)

data Program a = Prog 
  { pgFuns   :: [Func a]        -- ^ function defs
  , pgMain   :: !Id             -- ^ "main" 
  }
  deriving (Eq, Ord, Show, Functor)


class Labeled t where 
    getLabel :: t a -> a 

instance Labeled LVal where 
    getLabel (LvId  _   l) = l
    getLabel (LvFld _ _ l) = l

instance Labeled Expr where 
    getLabel (ELv   _   l) = l
    getLabel (ECall _ _ l) = l
    getLabel (ENew      l) = l

instance Labeled Stmt where 
    getLabel (SAsn  _ _   l) = l
    getLabel (SSeq  _     l) = l
    getLabel (SIf   _ _ _ l) = l
    getLabel (SWhl  _ _   l) = l
    getLabel (SRet  _     l) = l

instance Labeled Func where 
    getLabel = fnLabel

type BareExpr    = Expr SourceSpan
type BareLVal    = LVal SourceSpan
type BareStmt    = Stmt SourceSpan 
type BareFunc    = Func SourceSpan
type BareProgram = Program SourceSpan