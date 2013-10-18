-- | Translating lambda expressions.
module Language.BLC.Compile
    ( compile
    , compileMain
    ) where

import           Language.BLC.Core
import           Language.BLC.Encoding
import qualified Language.BLC.Parse    as P

-- | Compile a syntax tree into an expression. Chars are encoded as
-- 8-element lists of Church booleans, and strings are encoded as lists of
-- encoded chars.
compile :: P.Expr -> Expr String
compile = go
  where
    go (P.Var a)     = Var a
    go (P.App f as)  = app (go f) (map go as)
    go (P.Lam as e)  = foldr lam (go e) as
    go (P.CharLit c) = encodeChar c
    go (P.StrLit s)  = encodeString s
    go (P.Let ds b)  = foldr bind (go b) ds

    bind (P.Decl n s) e = App (lam n e) (go s)

-- | Compile the main module, wrapping top-level declarations in
-- @let <decls> in main;@.
compileMain :: [P.Decl] -> Expr String
compileMain decls = compile $ P.Let decls (P.Var "main")
