{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module LambdaMuMuCalculus.Typed.Syntax where

import           Data.String                       (IsString)

import qualified LambdaMuMuCalculus.Untyped.Syntax as Untyped

type Var = Untyped.Var
type CoVar = Untyped.CoVar

newtype TypeVar = TypeVar { getTypeVar :: Var }
  deriving newtype (Eq, Show, IsString)

instance Enum TypeVar where
  succ = TypeVar . succ . getTypeVar
  toEnum n = (!! n) . concat . drop 1 . iterate (map succ)
    $ ["A", "B", "C", "D"]
  fromEnum = error "fromEnum is not implemented for TypeVar"

type TypedTerm'    = TypedTerm    TypeVar CoVar Var
type TypedContext' = TypedContext TypeVar CoVar Var
type TypedCommand' = TypedCommand TypeVar CoVar Var

type Term'    = Term    TypeVar CoVar Var
type Context' = Context TypeVar CoVar Var
type Command' = Command TypeVar CoVar Var

data AType ty
  = TypeVariable ty
  | TypeFunction (AType ty) (AType ty)
  deriving (Eq)

data TypedTerm ty covar var
  = TypedTerm [(var, AType ty)] (Term ty covar var, AType ty) [(covar, AType ty)]

data TypedContext ty covar var
  = TypedContext [(var, AType ty)] (Context ty covar var, AType ty) [(covar, AType ty)]

data TypedCommand ty covar var
  = TypedComand (Command ty covar var) [(var, AType ty)] [(covar, AType ty)]

-- | A typed \(\bar{\lambda}\mu\tilde{\mu}\)-term.
data Term ty covar var
  = Variable var                                          -- ^ \( x \)
  | Lambda (var, AType ty) (Term ty covar var)
  | Mu (covar, AType ty) (Command ty covar var)
  deriving (Eq, Functor, Foldable)

-- | An untyped \(\bar{\lambda}\mu\tilde{\mu}\)-context.
data Context ty covar var
  = Covariable covar
  | App (Term ty covar var) (Context ty covar var)
  | MuVar (var, AType ty) (Command ty covar var)
  deriving (Eq, Functor, Foldable)

-- | An untyped \(\bar{\lambda}\mu\tilde{\mu}\)-command.
data Command ty covar var =
  Command (Term ty covar var) (Context ty covar var)
  deriving (Eq, Functor, Foldable)
