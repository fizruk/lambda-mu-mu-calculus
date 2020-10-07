{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
module LambdaMuMuCalculus.Typed.Colored where

import qualified Data.List                         as List
import           Data.String                       (IsString (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           LambdaMuMuCalculus.Typed.Syntax
import           LambdaMuMuCalculus.Untyped.Syntax (getVar)

import qualified System.Console.ANSI               as ANSI

newtype ColoredText = ColoredText { printColoredText :: IO () }
  deriving (Semigroup, Monoid)

instance IsString ColoredText where
  fromString = ColoredText . putStr

fromText :: Text -> ColoredText
fromText = fromString . Text.unpack

colored :: ANSI.ColorIntensity -> ANSI.Color -> ColoredText -> ColoredText
colored i c (ColoredText t) = ColoredText $ do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground i c]
  t
  ANSI.setSGR [ANSI.Reset]

class ColoredPretty a where ppColored :: a -> ColoredText

printColored :: ColoredPretty a => a -> IO ()
printColored = printColoredText . (<> "\n") . ppColored

instance ColoredPretty Term'      where ppColored = ppTerm
instance ColoredPretty Command'   where ppColored = ppCommand
instance ColoredPretty Context'   where ppColored = ppContext
instance ColoredPretty TypedTerm' where ppColored = ppTypedTerm

instance {-# OVERLAPPABLE #-} Show a => ColoredPretty a where
  ppColored = fromString . show

intercalate :: ColoredText -> [ColoredText] -> ColoredText
intercalate sep = mconcat . List.intersperse sep

ppTypedTerm :: TypedTerm' -> ColoredText
ppTypedTerm (TypedTerm vars (t, tt) covars)
  = ppVars vars <> " ⊢ " <> ppTerm t <> " : " <> ppType tt <> " | " <> ppCovars covars

ppVars :: [(Var, AType TypeVar)] -> ColoredText
ppVars = intercalate ", " . map ppTypedVar

ppCovars :: [(Var, AType TypeVar)] -> ColoredText
ppCovars = intercalate ", " . map ppTypedCovar

ppVar :: Var -> ColoredText
ppVar = colored ANSI.Vivid ANSI.Blue . fromText . getVar

ppCovar :: Var -> ColoredText
ppCovar = colored ANSI.Vivid ANSI.Magenta . fromText . getVar

ppTypedVar :: (Var, AType TypeVar) -> ColoredText
ppTypedVar (x, t) = ppVar x <> " : " <> ppType t

ppTypedCovar :: (Var, AType TypeVar) -> ColoredText
ppTypedCovar (x, t) = ppCovar x <> " : " <> ppType t

ppTerm :: Term' -> ColoredText
ppTerm (Variable x) = ppVar x
ppTerm (Lambda (x, xty) t) = "λ(" <> ppVar x <> " : " <> ppType xty <> ")." <> ppTerm t
ppTerm (Mu (alpha, aty) c) = "µ(" <> ppCovar alpha <> " : " <> ppType aty <> ")." <> ppCommand c

ppCommand :: Command' -> ColoredText
ppCommand (Command t e) = "⟨" <> ppTerm t <> "|" <> ppContext e <> "⟩"

ppContext :: Context' -> ColoredText
ppContext (Covariable alpha) = ppCovar alpha
ppContext (App t e)          = ppTerm t <> "·" <> ppContext e
ppContext (MuVar (x, xty) c) = "μ̃(" <> ppVar x <> " : " <> ppType xty <> ")." <> ppCommand c

ppType :: Show ty => AType ty -> ColoredText
ppType = colored ANSI.Vivid ANSI.Yellow . ppType'
  where
    ppType' (TypeVariable t)                     = fromString (show t)
    ppType' (TypeFunction a@(TypeVariable _) b)  = ppType' a <> " → " <> ppType' b
    ppType' (TypeFunction a b)                   = "(" <> ppType' a <> ") → " <> ppType' b

