-- | Translating parsed syntax trees into expressions.
module Language.BLC.Translate
    ( translate
    , construct
      -- * Encoding
    , encodeBool
    , decodeBool
    , encodeList
    , decodeList
    )  where

import           Bound
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Word

import           Language.BLC.Core
import qualified Language.BLC.Parse as P

-- | Extract an element out of a 'Right' value, or throw an error.
fromRight :: Either a b -> b
fromRight (Left _)  = error "fromRight: Left"
fromRight (Right b) = b

-- | Translate a syntax tree into an expression. Chars are encoded as
-- 8-element lists of Church booleans, and strings are encoded as lists of
-- encoded chars.
translate :: P.Expr -> Expr String
translate (P.Var a)     = Var a
translate (P.App f as)  = app (translate f) (map translate as)
translate (P.Lam as e)  = foldr lam (translate e) as
translate (P.CharLit c) = encodeChar c
translate (P.StrLit s)  = encodeString s
translate (P.Let ds b)  = foldr bind (translate b) ds
  where
    bind (P.Decl n s) e = App (lam n e) (translate s)
    bind (P.Import _) e = e

-- | Construct a closed lambda expression from a string. This will throw
-- an error if the expression could not be parsed, or is not closed.
construct :: String -> Expr a
construct = fromJust . closed . translate . fromRight . P.parseExpr

-- | Church encoding of 'True'.
true :: Expr a
true = construct "\\x y. x"

-- | Church encoding of 'False'.
false :: Expr a
false = construct "\\x y. y"

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
encodeList = foldr lcons lnil
  where
    lcons a b = app (construct "\\a b f. f a b") [a, b]
    lnil      = false

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

-- | Encode a 'String' as a list of encoded 'Char's.
encodeString :: String -> Expr a
encodeString = encodeList . map encodeChar
