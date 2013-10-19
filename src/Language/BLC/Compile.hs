-- | Translating lambda expressions.
module Language.BLC.Compile
    ( compile
    , compileFile
    , run
    , runIO
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

-- | Compile a file, which is a list of declarations. The compiled expression
-- is equivalent to @let <decls> in main;@. If the expression could not be
-- parsed, a 'ParseError' is thrown.
compileFile :: FilePath -> IO (Expr String)
compileFile fileName = do
    contents <- readFile fileName
    case P.parseDecls fileName contents of
        Left err -> fail (show err)
        Right ds -> return (compileMain ds)

-- | Run an expression, applying it to a string and returning the string
-- output.
run :: Eq a => Expr a -> String -> String
run e = decodeString . App e . encodeString

-- | Run an expression, applying it to @stdin@ and putting the result to
-- @stdout@.
runIO :: Eq a => Expr a -> IO ()
runIO = interact . run
