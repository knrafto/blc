{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Generating arbitrary expressions.
module Arbitrary
    ( V(..)
    ) where

import Control.Applicative
import Test.QuickCheck

import Language.BLC.Core

newtype V = V Char deriving (Eq, Ord, Show, Read)

instance Arbitrary V where
    arbitrary = V <$> elements "xyz"

instance (Eq a, Arbitrary a) => Arbitrary (Expr a) where
    arbitrary = sized arbExpr

arbExpr :: (Eq a, Arbitrary a) => Int -> Gen (Expr a)
arbExpr n = frequency $
    [ (1, Var <$> arbitrary) ] ++
    [ (3, App <$> arbExpr n2 <*> arbExpr n2) | n > 0] ++
    [ (3, lam <$> arbitrary <*> arbExpr n) | n > 0]
  where
    n2 = n `div` 2
