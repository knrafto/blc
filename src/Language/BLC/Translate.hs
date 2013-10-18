-- | Translating lambda expressions.
module Language.BLC.Translate
    ( translate
    ) where

import           Data.Map              (Map)
import qualified Data.Map              as Map

import           Language.BLC.Core
import           Language.BLC.Encoding
import qualified Language.BLC.Parse    as P

type ModuleMap = Map P.ModuleName P.Module

-- | Translate a syntax tree into an expression. Chars are encoded as
-- 8-element lists of Church booleans, and strings are encoded as lists of
-- encoded chars.
translate :: ModuleMap -> P.Expr -> Expr String
translate modules = go
  where
    go (P.Var a)     = Var a
    go (P.App f as)  = app (go f) (map go as)
    go (P.Lam as e)  = foldr lam (go e) as
    go (P.CharLit c) = encodeChar c
    go (P.StrLit s)  = encodeString s
    go (P.Let ds b)  = foldr bind (go b) ds

    bind (P.Decl n s) e = App (lam n e) (go s)
    bind (P.Import n) e = case Map.lookup n modules of
        Just m  -> foldr bind e (P.moduleDecls m)
        Nothing -> e
