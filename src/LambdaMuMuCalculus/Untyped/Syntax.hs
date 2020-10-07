{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module LambdaMuMuCalculus.Untyped.Syntax where

import           Data.Char
import           Data.Coerce   (coerce)
import           Data.Function (on)
import           Data.List     ((\\))
import           Data.Maybe    (fromMaybe)
import           Data.String   (IsString)
import           Data.Text     (Text)
import qualified Data.Text     as Text

-- | Standard type for covariables.
type CoVar = Var

-- | Standard type variables.
newtype Var = Var { getVar :: Text }
  deriving (Eq, IsString)

instance Show Var where show = Text.unpack . getVar

-- | Enum instance allows to easily increment index for a variable:
--
-- >>> succ "x" :: Var
-- x₁
instance Enum Var where
  succ = incVarIndex
  toEnum = error "toEnum is not implemented for Var"
  fromEnum = error "toEnum is not implemented for Var"

-- | Increment index for a variable.
--
-- >>> incVarIndex "x"
-- x₁
incVarIndex :: Var -> Var
incVarIndex = coerce incIndexText

-- | Standard term.
type Term'    = Term    CoVar Var

-- | Standard context.
type Context' = Context CoVar Var

-- | Standard command.
type Command' = Command CoVar Var

-- | An untyped \(\bar{\lambda}\mu\tilde{\mu}\)-term.
data Term covar var
  = Variable var                    -- ^ \( x \)
  | Lambda var (Term covar var)     -- ^ \( \lambda x. t \)
  | Mu covar (Command covar var)    -- ^ \( \mu \alpha. c \)
  deriving (Eq, Functor, Foldable)

-- | An untyped \(\bar{\lambda}\mu\tilde{\mu}\)-context.
data Context covar var
  = Covariable covar                          -- ^ \( \alpha \)
  | App (Term covar var) (Context covar var)  -- ^ \( t \cdot e \)
  | MuVar var (Command covar var)             -- ^ \( \tilde{\mu} x. c \)
  deriving (Eq, Functor, Foldable)

-- | An untyped \(\bar{\lambda}\mu\tilde{\mu}\)-command.
data Command covar var =
  Command (Term covar var) (Context covar var)  -- ^ \( \langle t | e \rangle \)
  deriving (Eq, Functor, Foldable)

-- | Alpha-equivalence of terms, contexts and commands.
class AlphaEquiv a where
  (===) :: a -> a -> Bool

-- |
-- >>> "λx.λy.y" === ("λa.λb.b" :: Term')
-- True
instance AlphaEquiv Term'     where (===) = (==) `on` (alphaNormalize          ("x", "α"))

-- |
-- >>> "μ̃x.⟨s|x·α⟩" === ("μ̃y.⟨s|y·α⟩" :: Context')
-- True
instance AlphaEquiv Context'  where (===) = (==) `on` (alphaNormalizeInContext ("x", "α"))

-- |
-- >>> "⟨µα.⟨s|z·δ⟩|μ̃x.⟨s|x·δ⟩⟩" === ("⟨µβ.⟨s|z·δ⟩|μ̃y.⟨s|y·δ⟩⟩" :: Untyped.Command')
-- True
instance AlphaEquiv Command'  where (===) = (==) `on` (alphaNormalizeInCommand ("x", "α"))

getBoundedVars :: Term covar var -> [var]
getBoundedVars (Variable _x) = []
getBoundedVars (Lambda x t)  = x : getBoundedVars t
getBoundedVars (Mu _a c)     = getBoundedVarsInCommand c

getBoundedVarsInCommand :: Command covar var -> [var]
getBoundedVarsInCommand (Command t e) = getBoundedVars t <> getBoundedVarsInContext e

getBoundedVarsInContext :: Context covar var -> [var]
getBoundedVarsInContext (Covariable _) = []
getBoundedVarsInContext (App t e) = getBoundedVars t <> getBoundedVarsInContext e
getBoundedVarsInContext (MuVar x c) = x : getBoundedVarsInCommand c

refreshVar :: (Eq var, Enum var) => [var] -> var -> var
refreshVar vars x
  | x `elem` vars = refreshVar vars (succ x)
  | otherwise     = x

getFreeVars :: (Eq var, Eq covar) => Term covar var -> ([var], [covar])
getFreeVars (Variable x) = ([x], [])
getFreeVars (Lambda x t) = (vars \\ [x], covars)
  where (vars, covars) = getFreeVars t
getFreeVars (Mu a c) = (vars, covars \\ [a])
  where (vars, covars) = getFreeVarsInCommand c

getFreeVarsInCommand :: (Eq var, Eq covar) => Command covar var -> ([var], [covar])
getFreeVarsInCommand (Command t e)
  = (tvars <> (evars \\ tvars), tcovars <> (ecovars \\ tcovars))
  where
    (tvars, tcovars) = getFreeVars t
    (evars, ecovars) = getFreeVarsInContext e

getFreeVarsInContext :: (Eq var, Eq covar) => Context covar var -> ([var], [covar])
getFreeVarsInContext (Covariable a) = ([], [a])
getFreeVarsInContext (App t e)
  = (tvars <> (evars \\ tvars), tcovars <> (ecovars \\ tcovars))
  where
    (tvars, tcovars) = getFreeVars t
    (evars, ecovars) = getFreeVarsInContext e
getFreeVarsInContext (MuVar x c) = (vars \\ [x], covars)
  where (vars, covars) = getFreeVarsInCommand c

concatMap' :: (a -> ([b], [c])) -> [a] -> ([b], [c])
concatMap' f = foldr (+++) ([], []) . map f
  where
    (xs, ys) +++ (xs', ys') = (xs ++ xs', ys ++ ys')

substitute
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Term covar var -> Term covar var
substitute vars covars = substitute' (fvars, fcovars) vars covars
  where
    (fvars, fcovars) = concatMap' id
      [ concatMap' (getFreeVars . snd) vars
      , concatMap' (getFreeVarsInContext . snd) covars
      ]

substituteInCommand
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Command covar var -> Command covar var
substituteInCommand vars covars = substituteInCommand' (fvars, fcovars) vars covars
  where
    (fvars, fcovars) = concatMap' id
      [ concatMap' (getFreeVars . snd) vars
      , concatMap' (getFreeVarsInContext . snd) covars
      ]

substituteInContext
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Context covar var -> Context covar var
substituteInContext vars covars = substituteInContext' (fvars, fcovars) vars covars
  where
    (fvars, fcovars) = concatMap' id
      [ concatMap' (getFreeVars . snd) vars
      , concatMap' (getFreeVarsInContext . snd) covars
      ]

substitute'
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => ([var], [covar])
  -> [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Term covar var -> Term covar var
substitute' fvs vars covars = go fvs
  where
    go (fvars, fcovars) = \case
      t@(Variable x) -> fromMaybe t (lookup x vars)
      Lambda x t
        | x `elem` fvars -> Lambda x' (substitute' (x' : fvars, fcovars) vars  covars (renameVar x x' t))
        | otherwise      -> Lambda x  (substitute' (x  : fvars, fcovars) vars' covars t)
        where
          x' = refreshVar fvars x
          vars' = filter ((/= x) . fst) vars
      Mu a c
        | a `elem` fcovars -> Mu a' (substituteInCommand' (fvars, a' : fcovars) vars covars  (renameCovarInCommand a a' c))
        | otherwise        -> Mu a  (substituteInCommand' (fvars, a  : fcovars) vars covars' c)
        where
          a' = refreshVar fcovars a
          covars' = filter ((/= a) . fst) covars

substituteInCommand'
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => ([var], [covar])
  -> [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Command covar var -> Command covar var
substituteInCommand' fvs vars covars (Command t e)
  = Command (substitute' fvs vars covars t) (substituteInContext' fvs vars covars e)

substituteInContext'
  :: (Eq var, Enum var, Eq covar, Enum covar)
  => ([var], [covar])
  -> [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Context covar var -> Context covar var
substituteInContext' fvs vars covars = go fvs
  where
    go (fvars, fcovars) = \case
      t@(Covariable a) -> fromMaybe t (lookup a covars)
      App t e -> App (substitute' fvs vars covars t) (substituteInContext' fvs vars covars e)
      MuVar x c
        | x `elem` fvars -> MuVar x' (substituteInCommand' (x' : fvars, fcovars) vars  covars (renameVarInCommand x x' c))
        | otherwise      -> MuVar x  (substituteInCommand' (x  : fvars, fcovars) vars' covars c)
        where
          x' = refreshVar fvars x
          vars' = filter ((/= x) . fst) vars


renameVar :: (Eq var, Eq covar) => var -> var -> Term covar var -> Term covar var
renameVar from to = substituteWithCapture [(from, Variable to)] []

renameVarInCommand :: (Eq var, Eq covar) => var -> var -> Command covar var -> Command covar var
renameVarInCommand from to = substituteWithCaptureInCommand [(from, Variable to)] []

renameVarInContext :: (Eq var, Eq covar) => var -> var -> Context covar var -> Context covar var
renameVarInContext from to = substituteWithCaptureInContext [(from, Variable to)] []

renameCovar :: (Eq var, Eq covar) => covar -> covar -> Term covar var -> Term covar var
renameCovar from to = substituteWithCapture [] [(from, Covariable to)]

renameCovarInCommand :: (Eq var, Eq covar) => covar -> covar -> Command covar var -> Command covar var
renameCovarInCommand from to = substituteWithCaptureInCommand [] [(from, Covariable to)]

renameCovarInContext :: (Eq var, Eq covar) => covar -> covar -> Context covar var -> Context covar var
renameCovarInContext from to = substituteWithCaptureInContext [] [(from, Covariable to)]

substituteWithCapture
  :: (Eq var, Eq covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Term covar var -> Term covar var
substituteWithCapture vars _ t@(Variable x) = fromMaybe t (lookup x vars)
substituteWithCapture vars covars (Lambda x t) = Lambda x (substituteWithCapture vars' covars t)
  where
    vars' = filter ((/= x) . fst) vars
substituteWithCapture vars covars (Mu a c) = Mu a (substituteWithCaptureInCommand vars covars' c)
  where
    covars' = filter ((/= a) . fst) covars

substituteWithCaptureInCommand
  :: (Eq var, Eq covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Command covar var -> Command covar var
substituteWithCaptureInCommand vars covars (Command term ctx)
  = Command (substituteWithCapture vars covars term) (substituteWithCaptureInContext vars covars ctx)

substituteWithCaptureInContext
  :: (Eq var, Eq covar)
  => [(var, Term covar var)]
  -> [(covar, Context covar var)]
  -> Context covar var -> Context covar var
substituteWithCaptureInContext _ covars e@(Covariable a) = fromMaybe e (lookup a covars)
substituteWithCaptureInContext vars covars (App t e) = App (substituteWithCapture vars covars t) (substituteWithCaptureInContext vars covars e)
substituteWithCaptureInContext vars covars (MuVar x c) = MuVar x (substituteWithCaptureInCommand vars' covars c)
  where
    vars' = filter ((/= x) . fst) vars

with :: (Eq var, Eq covar, Enum var, Enum covar) => Term covar var -> [(var, Term covar var)] -> Term covar var
with = flip (flip substitute [])

incIndexText :: Text -> Text
incIndexText s = name <> newIndex
  where
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    digitFromSub c = chr ((ord c - ord '₀') + ord '0')
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    (name, index) = Text.break isDigitSub s
    oldIndexN = read ('0' : map digitFromSub (Text.unpack index))
    newIndex = Text.pack (map digitToSub (show (oldIndexN + 1)))

alphaNormalize
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> Term covar var -> Term covar var
alphaNormalize xa t = alphaNormalize' xa (getFreeVars t) t

alphaNormalizeInCommand
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> Command covar var -> Command covar var
alphaNormalizeInCommand xa t = alphaNormalizeInCommand' xa (getFreeVarsInCommand t) t

alphaNormalizeInContext
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> Context covar var -> Context covar var
alphaNormalizeInContext xa t = alphaNormalizeInContext' xa (getFreeVarsInContext t) t

alphaNormalize'
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> ([var], [covar]) -> Term covar var -> Term covar var
alphaNormalize' (x, a) (vars, covars) = \case
  t@(Variable _) -> t
  Lambda y u     ->
    let y' = refreshVar vars x
     in Lambda y' (alphaNormalize' (x, a) (y' : vars, covars) (renameVar y y' u))
  Mu b c  ->
    let b' = refreshVar covars a
     in Mu b' (alphaNormalizeInCommand' (x, a) (vars, b' : covars) (renameCovarInCommand b b' c))

alphaNormalizeInCommand'
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> ([var], [covar]) -> Command covar var -> Command covar var
alphaNormalizeInCommand' xa fvs (Command t e)
  = Command (alphaNormalize' xa fvs t) (alphaNormalizeInContext' xa fvs e)

alphaNormalizeInContext'
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (var, covar) -> ([var], [covar]) -> Context covar var -> Context covar var
alphaNormalizeInContext' xa@(x, a) fvs@(vars, covars) = \case
  e@(Covariable _) -> e
  App t e -> App (alphaNormalize' xa fvs t) (alphaNormalizeInContext' xa fvs e)
  MuVar y c  ->
    let y' = refreshVar vars x
     in MuVar y' (alphaNormalizeInCommand' (x, a) (y' : vars, covars) (renameVarInCommand y y' c))

