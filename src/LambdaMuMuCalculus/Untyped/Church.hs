{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module LambdaMuMuCalculus.Untyped.Church where

import           Data.List.NonEmpty
import           LambdaMuMuCalculus.Untyped.Parser ()
import           LambdaMuMuCalculus.Untyped.Syntax
import           Numeric.Natural

instance Num Term' where
  (+) = app2 add
  (*) = app2 mul
  fromInteger = nat . fromInteger

true :: Term'
true = "λt.λf.t"

false :: Term'
false = "λt.λf.f"

zero :: Term'
zero = "λs.λz.z"

apps :: NonEmpty Term' -> Term'
apps = foldl1 app

app :: Term' -> Term' -> Term'
app f x = "µα.⟨x|μ̃x.⟨f|x·α⟩⟩" `with` [("x", x), ("f", f)]

app2 :: Term' -> Term' -> Term' -> Term'
app2 f x y = app (app f x) y

nat :: Natural -> Term'
nat m = substituteWithCapture [("t", nat' m)] [] "λs.λz.t"
  where
    nat' :: Natural -> Term'
    nat' n
      | n == 0    = "z"
      | otherwise = app "s" (nat' (n-1))

add :: Term'
add = "λn.λm.λs.λz.μα.⟨n|s·μβ.⟨m|s·z·β⟩·α⟩"

mul :: Term'
mul = "λn.λm.λs.μα.⟨n|μβ.⟨m|s·β⟩·α⟩"

pow :: Term'
pow = "λn.λm.μα.⟨m|n·α⟩"

