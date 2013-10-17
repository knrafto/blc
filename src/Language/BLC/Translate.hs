-- | Translating lambda expressions.
module Language.BLC.Translate
    ( translate
    ) where

import           Language.BLC.Core
import           Language.BLC.Encoding
import qualified Language.BLC.Parse    as P

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
