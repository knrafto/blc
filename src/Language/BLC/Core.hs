{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | An implementation of the lambda calculus.
module Language.BLC.Core
    ( Expr(..)
      -- * Construction
    , lam
    , app
      -- * Reduction
    , reduce
    ) where

import Prelude             hiding (sequence)

import Bound
import Control.Applicative
import Control.Monad       hiding (sequence)
import Data.Foldable       (Foldable)
import Data.Traversable
import Prelude.Extras

-- | A lambda calculus expression, parameterized by the type of its variables.
data Expr a
    = Var a
    | App (Expr a) (Expr a)
    | Lam (Scope () Expr a)
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr
instance Read1 Expr

instance Applicative Expr where
    pure  = return
    (<*>) = ap

instance Monad Expr where
    return = Var
    Var a    >>= f = f a
    App l r  >>= f = App (l >>= f) (r >>= f)
    Lam body >>= f = Lam (body >>>= f)

-- | Create a lambda abstraction.
lam :: Eq a => a -> Expr a -> Expr a
lam v b = Lam (abstract1 v b)

-- | Apply a function to a list of arguments.
app :: Expr a -> [Expr a] -> Expr a
app l = foldl App l

-- | Reduce an expression to beta-eta normal form.
reduce :: Expr a -> Expr a
reduce e@(Var _) = e
reduce (App f a) = beta f a
reduce (Lam s)   = eta s

-- | Apply the beta reduction rule, if possible, using a
-- call-by-need evaluation strategy
beta :: Expr a -> Expr a -> Expr a
beta f a = case f' of
    Lam b -> reduce (instantiate1 a' b)
    _     -> App f' a'
  where
    (f', a') = (reduce f, reduce a)

-- | Apply eta conversion, if possible.
eta :: Scope () Expr a -> Expr a
eta s = case unscope s of
    App a (Var (B ())) -> maybe (Lam s) reduce $ traverse fromVar a
    _                  -> (Lam s)
  where
    fromVar (F (Var a)) = Just a
    fromVar _           = Nothing
