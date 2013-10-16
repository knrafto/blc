{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where

import Prelude               hiding (notElem)

import Control.Applicative
import Data.Foldable         (notElem)
import Data.Function
import Test.ChasingBottoms
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

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

timeout :: Testable p => p -> Property
timeout p = not (isBottomTimeOut (Just 1) p) ==> p

reducible :: Expr V -> Bool
reducible e = reduce e /= e

(~=) :: Expr V -> Expr V -> Bool
(~=) = on (==) reduce

tests :: TestTree
tests = testGroup "tests" [properties]

properties :: TestTree
properties = testGroup "reduction"
    [ testProperty "idempotence" $
      \e -> timeout $ reduce e ~= e
    , testProperty "beta reduction" $
      \v e a -> timeout $ reducible (App (lam v e) a)
    , testProperty "eta reduction" $
      \v e -> timeout $ v `notElem` e ==> lam v (App e (Var v)) ~= e
    ]

main :: IO ()
main = defaultMain tests
