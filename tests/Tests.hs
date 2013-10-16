module Main
    ( main
    ) where

import Prelude               hiding (notElem)

import Data.Foldable         (notElem)
import Data.Function
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import Language.BLC.Core

import Arbitrary
import Infinite

guardFinite :: Testable p => p -> Property
guardFinite p = isFinite p ==> p

reducible :: Expr V -> Bool
reducible e = reduce e /= e

(~=) :: Expr V -> Expr V -> Bool
(~=) = on (==) reduce

tests :: TestTree
tests = testGroup "tests" [properties]

properties :: TestTree
properties = testGroup "reduction"
    [ testProperty "idempotence" $
      \e -> guardFinite $ reduce e ~= e
    , testProperty "beta reduction" $
      \v e a -> guardFinite $ reducible (App (lam v e) a)
    , testProperty "eta reduction" $
      \v e -> v `notElem` e ==> guardFinite $ lam v (App e (Var v)) ~= e
    ]

main :: IO ()
main = defaultMain tests
