{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module LambdaMuMuCalculus.Typed.TypeChecker where

import           Control.Arrow                     (second)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text

import           LambdaMuMuCalculus.Typed.Pretty   ()
import           LambdaMuMuCalculus.Typed.Syntax
import qualified LambdaMuMuCalculus.Untyped.Syntax as Untyped

data TypingContext ty covar var = TypingContext
  { freeVars      :: [(var, AType ty)]
  , freeCovars    :: [(covar, AType ty)]
  , knownTypes    :: [(ty, AType ty)]
  , freshTypeVars :: [ty]
  }

data TypeError ty
  = TypeErrorInfinite ty (AType ty)
  | TypeErrorUnexpected (AType ty) (AType ty)
  | TypeErrorOther Text

ppTypeError :: Show ty => TypeError ty -> Text
ppTypeError = \case
  TypeErrorInfinite x t ->
    "can't construct infinite type: " <> Text.pack (show x) <> " ~ " <> Text.pack (show t)
  TypeErrorUnexpected _ _ -> "unexpected type"
  TypeErrorOther msg -> msg

newtype TypeChecker ty covar var a = TypeChecker
  { runTypeChecker :: StateT (TypingContext ty covar var) (Except (TypeError ty)) a }
  deriving (Functor, Applicative, Monad, MonadState (TypingContext ty covar var), MonadError (TypeError ty))

checkInfiniteType :: Eq ty => AType ty -> ty -> AType ty -> TypeChecker ty covar var (AType ty)
checkInfiniteType tt x t@(TypeVariable y)
  | x == y && tt == t = return t
  | x == y    = throwError (TypeErrorInfinite x tt)
  | otherwise = do
      yt <- lookupTypeVar y
      case yt of
        Nothing -> return t
        Just t' -> do
          checkInfiniteType (instantiateTypes [(y, t')] tt) x t'
checkInfiniteType tt x (TypeFunction a b) = do
  a' <- checkInfiniteType tt x a
  b' <- checkInfiniteType tt x b
  return (TypeFunction a' b')

unify :: Eq ty => AType ty -> AType ty -> TypeChecker ty covar var ()
unify t1 t2 = do
  TypingContext{..} <- get
  let t1' = instantiateTypes knownTypes t1
      t2' = instantiateTypes knownTypes t2
  unify' t1' t2'
  where
    unify' (TypeFunction a b) (TypeFunction c d) = do
      unify' a c
      unify' b d
    unify' (TypeVariable x) (TypeVariable y)
      | x == y = return ()
    unify' (TypeVariable x) t = do
      mty <- lookupTypeVar x
      case mty of
        Nothing -> do
          t' <- checkInfiniteType t x t
          modify (\ctx -> ctx {
            freeVars = map (second (instantiateTypes [(x, t')])) (freeVars ctx),
            freeCovars = map (second (instantiateTypes [(x, t')])) (freeCovars ctx),
            knownTypes = (x, t') : map (second (instantiateTypes [(x, t')])) (knownTypes ctx)
            } )
        Just xty -> unify' xty t
    unify' t (TypeVariable x) = unify' (TypeVariable x) t
    -- unify' t1 t2 = throwError (TypeErrorUnexpected t1 t2)

instance MonadFail (TypeChecker ty covar var) where
  fail = throwError . TypeErrorOther . Text.pack

genFreshTypeVar :: TypeChecker ty covar var ty
genFreshTypeVar = do
  ctx@TypingContext{ freshTypeVars = t:ts } <- get
  put ctx { freshTypeVars = ts }
  return t

lookupVar :: Eq var => var -> TypeChecker ty covar var (Maybe (AType ty))
lookupVar x = gets (lookup x . freeVars)

lookupCovar :: Eq covar => covar -> TypeChecker ty covar var (Maybe (AType ty))
lookupCovar a = gets (lookup a . freeCovars)

lookupTypeVar :: Eq ty => ty -> TypeChecker ty covar var (Maybe (AType ty))
lookupTypeVar t = gets (lookup t . knownTypes)

addFreeVar :: Eq var => var -> TypeChecker ty covar var (AType ty)
addFreeVar x = do
  mty <- lookupVar x
  case mty of
    Just xty -> return xty
    Nothing -> do
      xty <- TypeVariable <$> genFreshTypeVar
      modify (\ctx -> ctx { freeVars = (x, xty) : freeVars ctx } )
      return xty

addFreeCovar :: Eq covar => covar -> TypeChecker ty covar var (AType ty)
addFreeCovar a = do
  mty <- lookupCovar a
  case mty of
    Just aty -> return aty
    Nothing -> do
      aty <- TypeVariable <$> genFreshTypeVar
      modify (\ctx -> ctx { freeCovars = (a, aty) : freeCovars ctx } )
      return aty

localVar :: Eq var => var -> (AType ty -> TypeChecker ty covar var a) -> TypeChecker ty covar var a
localVar x m = do
  old <- lookupVar x
  new <- TypeVariable <$> genFreshTypeVar
  modify (\ctx -> ctx { freeVars = (x, new) : filter ((/= x) . fst) (freeVars ctx) })
  res <- m new
  case old of
    Nothing -> modify (\ctx -> ctx { freeVars = filter ((/= x) . fst) (freeVars ctx) })
    Just old' -> modify (\ctx -> ctx { freeVars = (x, old') : filter ((/= x) . fst) (freeVars ctx) })
  return res

localCovar :: Eq covar => covar -> (AType ty -> TypeChecker ty covar var a) -> TypeChecker ty covar var a
localCovar a m = do
  old <- lookupCovar a
  new <- TypeVariable <$> genFreshTypeVar
  modify (\ctx -> ctx { freeCovars = (a, new) : filter ((/= a) . fst) (freeCovars ctx) })
  res <- m new
  case old of
    Nothing -> modify (\ctx -> ctx { freeCovars = filter ((/= a) . fst) (freeCovars ctx) })
    Just old' -> modify (\ctx -> ctx { freeCovars = (a, old') : filter ((/= a) . fst) (freeCovars ctx) })
  return res

typecheck
  :: (Eq var, Eq covar, Eq ty)
  => Untyped.Term covar var
  -> TypeChecker ty covar var (Term ty covar var, AType ty)
typecheck = \case
  Untyped.Variable x -> do
    xty <- addFreeVar x
    return (Variable x, xty)
  Untyped.Lambda x t -> do
    localVar x $ \xty -> do
      (body, bodyType) <- typecheck t
      return (Lambda (x, xty) body, TypeFunction xty bodyType)
  Untyped.Mu a c -> do
    localCovar a $ \aty -> do
      c' <- typecheckCommand c
      return (Mu (a, aty) c', aty)

typecheckCommand
  :: (Eq var, Eq covar, Eq ty)
  => Untyped.Command covar var -> TypeChecker ty covar var (Command ty covar var)
typecheckCommand (Untyped.Command t e) = do
  (t', tt) <- typecheck t
  (e', ee) <- typecheckContext e
  unify tt ee
  return (Command t' e')

typecheckContext
  :: (Eq var, Eq covar, Eq ty)
  => Untyped.Context covar var -> TypeChecker ty covar var (Context ty covar var, AType ty)
typecheckContext = \case
  Untyped.Covariable a -> do
    aty <- addFreeCovar a
    return (Covariable a, aty)
  Untyped.App t e -> do
    (t', tt) <- typecheck t
    (e', ee) <- typecheckContext e
    return (App t' e', TypeFunction tt ee)
  Untyped.MuVar x c -> do
    localVar x $ \xty -> do
      c' <- typecheckCommand c
      return (MuVar (x, xty) c', xty)

emptyTypingContext :: Enum ty => TypingContext ty covar var
emptyTypingContext = TypingContext
  { freeVars = []
  , freeCovars = []
  , knownTypes = []
  , freshTypeVars = map toEnum [0..]
  }

instantiateTypes :: Eq ty => [(ty, AType ty)] -> AType ty -> AType ty
instantiateTypes ts (TypeVariable x) = fromMaybe (TypeVariable x) (lookup x ts)
instantiateTypes ts (TypeFunction a b) = TypeFunction (instantiateTypes ts a) (instantiateTypes ts b)

mapTypes :: (AType ty -> AType ty) -> Term ty covar var -> Term ty covar var
mapTypes _ (Variable x)        = Variable x
mapTypes f (Lambda (x, xty) t) = Lambda (x, f xty) (mapTypes f t)
mapTypes f (Mu (a, aty) c)     = Mu (a, f aty) (mapTypesInCommand f c)

mapTypesInCommand :: (AType ty -> AType ty) -> Command ty covar var -> Command ty covar var
mapTypesInCommand f (Command t e) = Command (mapTypes f t) (mapTypesInContext f e)

mapTypesInContext :: (AType ty -> AType ty) -> Context ty covar var -> Context ty covar var
mapTypesInContext _ (Covariable a) = Covariable a
mapTypesInContext f (App t e) = App (mapTypes f t) (mapTypesInContext f e)
mapTypesInContext f (MuVar (x, xty) c) = MuVar (x, f xty) (mapTypesInCommand f c)

unsafeRunTypeChecker
  :: (Enum ty, Show ty)
  => TypeChecker ty covar var a -> (TypingContext ty covar var, a)
unsafeRunTypeChecker m =
  case runExcept (runStateT (runTypeChecker m) emptyTypingContext) of
    Left err       -> error ("TypeError: " <> Text.unpack (ppTypeError err))
    Right (x, ctx) -> (ctx, x)

unsafeTypecheck :: Untyped.Term' -> TypedTerm'
unsafeTypecheck = toTypedTerm . unsafeRunTypeChecker . typecheck
  where
    toTypedTerm (ctx, (t, tt)) = TypedTerm (freeVars ctx) (t', tt') (freeCovars ctx)
      where
        t' = mapTypes unfoldKnownTypes t
        tt' = unfoldKnownTypes tt
        unfoldKnownTypes = instantiateTypes (knownTypes ctx)

