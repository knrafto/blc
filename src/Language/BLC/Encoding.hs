-- | Encoding and decoding values to lambda expressions.
module Language.BLC.Encoding
    ( -- * Booleans
      encodeBool
    , decodeBool
      -- * Lists
    , encodeList
    , decodeList
      -- * Chars
    , encodeChar
    , decodeChar
      -- * Strings
    , encodeString
    , decodeString
    )  where

import Bound
import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Word

import Language.BLC.Core

-- | Take 'Just' elements until the first 'Nothing'.
takeJust :: [Maybe a] -> [a]
takeJust (Just x:xs) = x : takeJust xs
takeJust _           = []

-- | Construct a closed lambda expression, or throw an error if the expression
-- is not closed.
construct :: Expr a -> Expr b
construct = fromJust . closed

-- | Church encoding of 'True'.
true :: Expr a
true = construct $ lam 'x' (lam 'y' (Var 'x'))

-- | Church encoding of 'False'.
false :: Expr a
false = construct $ lam 'x' (lam 'y' (Var 'y'))

-- | Break a 'Word8' into its constituent bits, most significant first.
bits :: Word8 -> [Bool]
bits w = map (testBit w) [7,6..0]

-- | Convert a list of bits (most significant first) into a 'Word8'.
unbits :: [Bool] -> Word8
unbits = foldl' (\a b -> 2*a + if b then 1 else 0) 0

-- | Church-encode a 'Bool'.
encodeBool :: Bool -> Expr a
encodeBool b = if b then true else false

-- | Decode a church-encoded 'Bool'.
decodeBool :: Eq a => Expr a -> Maybe Bool
decodeBool e = case reduce e of
    e' | e' == true  -> Just True
       | e' == false -> Just False
    _                -> Nothing

-- | Encode a list as a recursive pairing. The empty list if represented by
-- false.
encodeList :: [Expr a] -> Expr a
encodeList = foldr (\a b -> app lcons [a, b]) lnil
  where
    lcons = construct $ lam 'a' (lam 'b' (lam 'f'
                        (app (Var 'f') [Var 'a', Var 'b'])))
    lnil  = false


-- | Decode a list.
decodeList :: Eq a => Expr a -> [Expr a]
decodeList = unfoldr f
  where
    f e = case reduce e of
        Lam (Scope (App (App (Var (B ())) (Var (F a))) (Var (F b))))
          -> Just (a, b)
        _ -> Nothing

-- | Encode a 'Char' as a list of Church booleans.
encodeChar :: Char -> Expr a
encodeChar = encodeList . map encodeBool . bits . fromIntegral . ord

-- | Decode a 'Char'.
decodeChar :: Eq a => Expr a -> Maybe Char
decodeChar e = do
    bs <- traverse decodeBool (decodeList e)
    guard (length bs == 8)
    return . chr . fromIntegral $ unbits bs

-- | Encode a 'String' as a list of encoded 'Char's.
encodeString :: String -> Expr a
encodeString = encodeList . map encodeChar

-- | Decode a 'String'.
decodeString :: Eq a => Expr a -> String
decodeString = takeJust . map decodeChar . decodeList
