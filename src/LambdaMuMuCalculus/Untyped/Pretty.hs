{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LambdaMuMuCalculus.Untyped.Pretty where

import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           LambdaMuMuCalculus.Untyped.Syntax

instance Show Term'     where show = Text.unpack . ppTerm
instance Show Command'  where show = Text.unpack . ppCommand
instance Show Context'  where show = Text.unpack . ppContext

ppTerm :: Term' -> Text
ppTerm (Variable x) = getVar x
ppTerm (Lambda x t) = "λ" <> getVar x <> "." <> ppTerm t
ppTerm (Mu alpha c) = "µ" <> getVar alpha <> "." <> ppCommand c

ppCommand :: Command' -> Text
ppCommand (Command t e) = "⟨" <> ppTerm t <> "|" <> ppContext e <> "⟩"

ppContext :: Context' -> Text
ppContext (Covariable alpha) = getVar alpha
ppContext (App t e)          = ppTerm t <> "·" <> ppContext e
ppContext (MuVar x c)        = "μ̃" <> getVar x <> "." <> ppCommand c

