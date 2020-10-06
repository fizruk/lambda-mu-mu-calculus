{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LambdaMuMuCalculus.Untyped.Parser where

import           Control.Applicative
import           Data.Attoparsec.Text              as Atto
import           Data.Char                         (chr, ord)
import           Data.List                         ((\\))
import           Data.String                       (IsString (..))
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           LambdaMuMuCalculus.Untyped.Syntax

instance IsString Term'    where fromString = unsafeParseTerm    . Text.pack
instance IsString Context' where fromString = unsafeParseContext . Text.pack
instance IsString Command' where fromString = unsafeParseCommand . Text.pack

unsafeParseTerm :: Text -> Term'
unsafeParseTerm = unsafeParse "term" term

unsafeParseContext :: Text -> Context'
unsafeParseContext = unsafeParse "context" context

unsafeParseCommand :: Text -> Command'
unsafeParseCommand = unsafeParse "command" command

command :: Parser Command'
command = do
  "<" <|> "⟨"
  t <- term
  "|"
  e <- context
  ">" <> "⟩"
  return (Command t e)

term :: Parser Term'
term = lambda
   <|> mu
   <|> (Variable <$> var)

context :: Parser Context'
context
   = app
 <|> muVar
 <|> (Covariable <$> covar)

lambda :: Parser Term'
lambda = do
  "λ" <|> "\\"
  x <- var <?> "variable"
  "." <|> "."
  t <- term <?> "term"
  return (Lambda x t)

mu :: Parser Term'
mu = do
  "μ" <|> "µ" <|> "M"
  alpha <- covar <?> "co-variable"
  "."
  c <- command <?> "command"
  return (Mu alpha c)

app :: Parser Context'
app = do
  t <- term
  "·" <|> ";"
  e <- context
  return (App t e)

muVar :: Parser Context'
muVar = do
  "μ̃" <|> "M'"
  x <- var <?> "variable"
  "."
  c <- command <?> "command"
  return (MuVar x c)

var :: Parser Var
var = do
  first <- satisfy (inClass (latinSmall ++ "_"))
  rest <- Atto.takeWhile (inClass (latinSmall ++ digits ++ digitsSub))
  return (Var (identToUnicode (Text.cons first rest)))
  where
    digits        = "0123456789"
    digitsSub     = "₀₁₂₃₄₅₆₇₈₉"
    latinSmall    = "abcdefghijklmnopqrstuvwxyz"

covar :: Parser CoVar
covar = do
  first <- satisfy (inClass (greekSmall ++ latinSmall))
  rest <- Atto.takeWhile (inClass (greekSmall ++ latinSmall ++ digits ++ digitsSub))
  return (Var (nameToUnicode (Text.cons first rest)))
  where
    digits        = "0123456789"
    digitsSub     = "₀₁₂₃₄₅₆₇₈₉"
    latinSmall    = "abcdefghijklmnopqrstuvwxyz"
    greekSmall    = "αβγδεζηθικλμνξοπρςστυφχψω" \\ "λμ"

latinToGreekChar :: Char -> Char
latinToGreekChar c =
  case lookup c (zip latinSmall greekSmall) of
    Just c' -> c'
    Nothing -> c
  where
    latinSmall    = "abcdefghijklmnopqrstuvwxyz"
    greekSmall    = "αβγδεζηθιξκξξνοπξρστυφωχψξ"

nameToUnicode :: Text -> Text
nameToUnicode = Text.map latinToGreekChar . identToUnicode

identToUnicode :: Text -> Text
identToUnicode s = prefix <> newIndex
  where
    (prefix, index) = Text.break isDigitOrDigitSub s

    digits    = "0123456789" :: String
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    isDigit    = (`elem` digits)
    isDigitOrDigitSub c = isDigit c || isDigitSub c
    digitFromSub c
      | isDigitSub c = chr ((ord c - ord '₀') + ord '0')
      | otherwise    = c
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

    oldIndexN = read ('0' : map digitFromSub (Text.unpack index)) :: Integer
    newIndex
      | Text.null index = ""
      | otherwise       = Text.pack (digitToSub <$> show oldIndexN)

unsafeParse :: String -> Parser a -> Text -> a
unsafeParse name parser input =
  case Atto.parseOnly (parser <* endOfInput) input of
    Right t  -> t
    Left err -> error $ unlines
      [ "Failed parsing " <> name
      , "    " <> Text.unpack input
      , "Parsing error was:"
      , err
      ]

