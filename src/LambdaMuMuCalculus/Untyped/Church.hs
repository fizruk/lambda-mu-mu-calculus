{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LambdaMuMuCalculus.Untyped.Church where

import           Data.List.NonEmpty
import           GHC.Exts                          (IsList (..))
import           LambdaMuMuCalculus.Untyped.Parser ()
import           LambdaMuMuCalculus.Untyped.Syntax
import           Numeric.Natural

instance Num Term' where
  (+) = app2 add
  (*) = app2 mul
  abs = id
  signum = app1 "λn.λs.λz.µα.⟨n|λz.λs.λz.µα.⟨z|μ̃x.⟨s|x·α⟩⟩·z·α⟩"
  negate = error "negate is not implemented for Term' Church numerals"
  (-)    = error "(-) is not implemented for Term' Church numerals"
  fromInteger = nat . fromInteger

-- * Function application

-- | Apply a term to a list of arguments.
--
-- >>> apps ("f" :| ["x", "y", "z"])
-- µα.⟨z|μ̃x₁.⟨µα.⟨y|μ̃x₁.⟨µα.⟨x|μ̃x₁.⟨f|x₁·α⟩⟩|x₁·α⟩⟩|x₁·α⟩⟩
apps :: NonEmpty Term' -> Term'
apps = foldl1 app1

app1 :: Term' -> Term' -> Term'
app1 f x = "µα.⟨f|x·α⟩" `with` [("x", x), ("f", f)]

app2 :: Term' -> Term' -> Term' -> Term'
app2 f x y = app1 (app1 f x) y

-- * Booleans

true :: Term'
true = "λt.λf.t"

false :: Term'
false = "λt.λf.f"

ifThenElse :: Term'
ifThenElse = "λc.λt.λf.µα.⟨f|μ̃x.⟨µα.⟨t|μ̃x.⟨c|x·α⟩⟩|x·α⟩⟩"

-- * Natural numbers

nat :: Natural -> Term'
nat m = substituteWithCapture [("t", nat' m)] [] "λs.λz.t"
  where
    nat' :: Natural -> Term'
    nat' n
      | n == 0    = "z"
      | otherwise = app1 "s" (nat' (n-1))

add :: Term'
add = "λn.λm.λs.λz.μα.⟨n|s·μβ.⟨m|s·z·β⟩·α⟩"

mul :: Term'
mul = "λn.λm.λs.μα.⟨n|μβ.⟨m|s·β⟩·α⟩"

pow :: Term'
pow = "λn.λm.μα.⟨m|n·α⟩"

-- * Non-determinism

-- | Choose one of two values (non-deterministically).
--
-- The choice depends on the evaluation strategy:
--
-- >>> nf cbv (choice 1 2)
-- µα₂.⟨s|z·α₂⟩
-- >>> nf cbn (choice 1 2)
-- µα.⟨s|µα₁.⟨s|z·α₁⟩·α⟩
choice :: Term' -> Term' -> Term'
choice c1 c2 = "µδ.⟨µα.⟨c₁|δ⟩|μ̃x.⟨c₂|δ⟩⟩" `with` [("c₁", c1), ("c₂", c2)]

-- | Choose one of many values (non-deterministically).
--
-- The choice depends on the evaluation strategy:
--
-- >>> nf cbv (choices (1 :| [2, 3, 4]))
-- λs.λz.µα.⟨s|z·α⟩
-- >>> nf cbn (choices (1 :| [2, 3, 4]))
-- λs.λz.µα.⟨s|µα₁.⟨s|µα₂.⟨s|µα₃.⟨s|z·α₃⟩·α₂⟩·α₁⟩·α⟩
choices :: NonEmpty Term' -> Term'
choices = foldr1 choice

-- * Church-encoded data structures

-- ** Pair

pair :: Term' -> Term' -> Term'
pair p1 p2 = substituteWithCapture [("z", app2 "p" p1 p2)] [] "λp.z"

first :: Term'
first = "λp.µα.⟨p|λx.λy.x·α⟩"

second :: Term'
second = "λp.µα.⟨p|λx.λy.y·α⟩"

-- ** Lists

nil :: Term'
nil = "λf.λz.z"

cons :: Term'
cons = "λh.λt.λf.λz.µα.⟨f|h·µβ.⟨t|f·z·β⟩·α⟩"

list :: [Term'] -> Term'
list zs = substituteWithCapture [("t", list' zs)] [] "λf.λz.t"
  where
    list' :: [Term'] -> Term'
    list' []     = "z"
    list' (x:xs) = "µα.⟨f|x·t·α⟩" `with` [("x", x), ("t", list' xs)]

instance IsList Term' where
  type Item Term' = Term'
  fromList = list
  toList = pure   -- maybe this does not make much sense
