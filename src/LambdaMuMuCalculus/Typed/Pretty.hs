{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LambdaMuMuCalculus.Typed.Pretty where

import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           LambdaMuMuCalculus.Typed.Syntax
import           LambdaMuMuCalculus.Untyped.Syntax (getVar)

instance Show Term'     where show = Text.unpack . ppTerm
instance Show Command'  where show = Text.unpack . ppCommand
instance Show Context'  where show = Text.unpack . ppContext

instance Show TypedTerm' where show = Text.unpack . ppTypedTerm

ppTypedTerm :: TypedTerm' -> Text
ppTypedTerm (TypedTerm vars (t, tt) covars)
  = ppVars vars <> " ⊢ " <> ppTerm t <> " : " <> ppType tt <> " | " <> ppVars covars

ppVars :: [(Var, AType TypeVar)] -> Text
ppVars = Text.intercalate ", " . map ppTypedVar

ppTypedVar :: (Var, AType TypeVar) -> Text
ppTypedVar (x, t) = getVar x <> " : " <> ppType t

ppTerm :: Term' -> Text
ppTerm (Variable x) = getVar x
ppTerm (Lambda (x, xty) t) = "λ(" <> getVar x <> " : " <> ppType xty <> ")." <> ppTerm t
ppTerm (Mu (alpha, aty) c) = "µ(" <> getVar alpha <> ":" <> ppType aty <> ")." <> ppCommand c

ppCommand :: Command' -> Text
ppCommand (Command t e) = "⟨" <> ppTerm t <> "|" <> ppContext e <> "⟩"

ppContext :: Context' -> Text
ppContext (Covariable alpha) = getVar alpha
ppContext (App t e)          = ppTerm t <> "·" <> ppContext e
ppContext (MuVar (x, xty) c) = "μ̃(" <> getVar x <> " : " <> ppType xty <> ")." <> ppCommand c

