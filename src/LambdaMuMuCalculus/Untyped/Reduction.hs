{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaMuMuCalculus.Untyped.Reduction where

import           Control.Applicative
import           Data.List.NonEmpty                as NonEmpty
import qualified Data.Text.IO                      as Text
import           LambdaMuMuCalculus.Untyped.Pretty
import           LambdaMuMuCalculus.Untyped.Syntax

-- | Call-by-value evaluation strategy.
cbv :: (Eq var, Eq covar, Enum var, Enum covar)
    => Command covar var -> Maybe (Command covar var)
cbv = \case
  Command (Lambda x t1) (App t2 e)  ->
    let (vars, _) = getFreeVarsInContext e
        x' = refreshVar vars x
        t1' = renameVar x x' t1
     in cbv (Command t2 (MuVar x' (Command t1' e)))
  Command (Mu b c) e                -> Just (substituteInCommand [] [(b, e)] c)
  Command t (MuVar x c)             -> Just (substituteInCommand [(x, t)] [] c)
  _                                 -> Nothing

-- | Call-by-name evaluation strategy.
cbn :: (Eq var, Eq covar, Enum var, Enum covar)
    => Command covar var -> Maybe (Command covar var)
cbn = \case
  Command (Lambda x t1) (App t2 e)  -> Just (Command (substitute [(x, t2)] [] t1) e)
  Command t (MuVar x c)             -> Just (substituteInCommand [(x, t)] [] c)
  Command (Mu b c) e                -> Just (substituteInCommand [] [(b, e)] c)
  _                                 -> Nothing

whnf
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (Command covar var -> Maybe (Command covar var))
  -> Command covar var -> Command covar var
whnf step = NonEmpty.last . iterateMaybe step

-- | Collect all reduction steps towards the normal form.
nfReductions
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (Command covar var -> Maybe (Command covar var))
  -> Term covar var -> NonEmpty (Term covar var)
nfReductions step = iterateMaybe (normalizeStep step)

-- | Pretty-print reductions step-by-step.
ppReductions :: (Command' -> Maybe Command') -> Term' -> IO ()
ppReductions step t =
  case nfReductions step t of
    t' :| ts -> do
      Text.putStrLn ("  " <> ppTerm t')
      mapM_ (Text.putStrLn . ("↦ " <>) . ppTerm) ts

-- | Compute normal form of a term, using given reduction strategy,
-- adding two \(\eta\)-reduction rules:
--
-- * \( \mu \alpha. \langle t | \alpha \rangle \mapsto t \)
-- * \( \tilde{\mu} x. \langle x | e \rangle \mapsto e \)
nf :: (Eq var, Eq covar, Enum var, Enum covar)
   => (Command covar var -> Maybe (Command covar var))
   -> Term covar var -> Term covar var
nf step = NonEmpty.last . iterateMaybe (normalizeStep step)

-- | Perform one reduction step using provided strategy,
-- adding two \(\eta\)-reduction rules:
--
-- * \( \mu \alpha. \langle t | \alpha \rangle \mapsto t \)
-- * \( \tilde{\mu} x. \langle x | e \rangle \mapsto e \)
normalizeStep
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (Command covar var -> Maybe (Command covar var))  -- Reduction strategy.
  -> Term covar var -> Maybe (Term covar var)
normalizeStep _    (Variable _) = Nothing
normalizeStep step (Lambda x t) = Lambda x <$> normalizeStep step t
normalizeStep _ (Mu a (Command t (Covariable a'))) | a == a' = Just t
normalizeStep step (Mu a c)     = Mu a <$> normalizeStepInCommand step c

-- | Like 'normalizeStep', but for 'Command'.
normalizeStepInCommand
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (Command covar var -> Maybe (Command covar var))  -- Reduction strategy.
  -> Command covar var -> Maybe (Command covar var)
normalizeStepInCommand step c@(Command t e)
    = step c
  <|> Command <$> normalizeStep step t <*> pure e
  <|> Command t <$> normalizeStepInContext step e

-- | Like 'normalizeStep', but for 'Context'.
normalizeStepInContext
  :: (Eq var, Eq covar, Enum var, Enum covar)
  => (Command covar var -> Maybe (Command covar var))  -- Reduction strategy.
  -> Context covar var -> Maybe (Context covar var)
normalizeStepInContext _ (Covariable _) = Nothing
normalizeStepInContext step (App t e)
    = App <$> normalizeStep step t <*> pure e
  <|> App t <$> normalizeStepInContext step e
normalizeStepInContext _ (MuVar x (Command (Variable x') e)) | x == x' = Just e
normalizeStepInContext step (MuVar x c)
  = MuVar x <$> normalizeStepInCommand step c

-- | A helper to iterate reduction while possible.
iterateMaybe :: (a -> Maybe a) -> a -> NonEmpty a
iterateMaybe f x = x :| go x
  where
    go y =
      case f y of
        Nothing -> []
        Just z  -> z : go z

