module Main
    ( main
    ) where

import Prelude                hiding (notElem)

import Data.Foldable          (notElem)
import Data.Function
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit       hiding (Testable)

import Language.BLC.Core
import Language.BLC.Parse     (parseExpr)
import Language.BLC.Translate

import Arbitrary
import Infinite

guardFinite :: Testable p => p -> Property
guardFinite p = isFinite p ==> p

reducible :: Expr V -> Bool
reducible e = reduce e /= e

(~=) :: Expr V -> Expr V -> Bool
(~=) = on (==) reduce


(@~=) :: String -> String -> Assertion
(@~=) s1 s2 = do
    e1 <- assertParse s1
    e2 <- assertParse s2
    reduce e1 @=? reduce e2
  where
    assertParse s = case parseExpr s of
        Left  _ -> assertFailure ("failed parse: " ++ s) >> undefined
        Right e -> return (translate e)

properties :: TestTree
properties = testGroup "properties"
    [ testGroup "reduction"
        [ testProperty "idempotence" $
          \e -> guardFinite $ reduce e ~= e
        , testProperty "beta reduction" $
          \v e a -> guardFinite $ reducible (App (lam v e) a)
        , testProperty "eta reduction" $
          \v e -> v `notElem` e ==>
            guardFinite $ lam v (App e (Var v)) ~= e
        ]
    , testGroup "encoding"
        [ testProperty "boolean" $
          \b -> decodeBool (encodeBool b :: Expr V) == Just b
        ]
    ]

unitTests :: TestTree
unitTests = testGroup "tests"
    [ testGroup "reduction"
        [ testCase "alpha equivalence" $ do
            "\\x. x" @~= "\\y. y"
            "\\x. (\\x. x) a" @~= "\\y. (\\z. z) a"
        , testCase "beta reduction" $ do
            "(\\x. x) (\\x. x)" @~= "\\x. x"
            "(\\x y. x) (\\a. a) ((\\x. x x) (\\x. x x))" @~= "\\a. a"
        , testCase "eta conversion" $ do
            "\\a. f a" @~= "f"
            "\\x. (\\x. z) x" @~= "\\x. z"
        , testCase "abstraction" $ do
            "\\x y z. a" @~= "\\x. \\y. \\z. a"
            "\\x. \\y. y x" @~= "\\x. (\\y. (y x))"
        , testCase "application" $ do
            "f a b c" @~= "((f a) b) c"
            "f \\x. x \\y. y" @~= "f (\\x. x (\\y. y))"
        , testCase "let" $ do
            "let a = b in a" @~= "b"
            "let x = a; y = x; x = b in x y" @~= "b a"
        ]
    ]

tests :: TestTree
tests = testGroup "tests" [properties, unitTests]

main :: IO ()
main = defaultMain tests
