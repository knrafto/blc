-- | A human-readable version of untyped lambda calculus.
module Language.BLC.Parse
    ( -- * Types
      Name
    , Decl(..)
    , Expr(..)
      -- * Parsing
    , parseExpr
    ) where

import           Control.Applicative    hiding (optional)
import           Control.Monad.Identity
import           Data.Char
import           Text.Parsec            hiding ((<|>), many)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token      as P

-- | A variable.
type Name = String

-- | A declaration or import statement.
data Decl = Decl Name Expr
    deriving (Eq, Ord, Show, Read)

-- | An expression.
data Expr
    = Var Name
    | App Expr [Expr]
    | Lam [Name] Expr
    | Let [Decl] Expr
    | CharLit Char
    | StrLit String
    deriving (Eq, Ord, Show, Read)

-- | Parse a value from a file name and string, consuming all input.
parseAll :: Parser a -> String -> String -> Either ParseError a
parseAll p = parse (P.whiteSpace lexer *> p <* eof)

-- | Parse an expression.
parseExpr :: String -> Either ParseError Expr
parseExpr = parseAll expr ""

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser emptyDef
    { P.commentLine   = "#"
    , P.identStart    = nameStart
    , P.identLetter   = nameLetter
    , P.reservedNames = ["import", "let", "in", "="]
    }
  where
    nameStart  = satisfy $ \c -> not $ isSpace c || c `elem` "\\.#();'\""
    nameLetter = satisfy $ \c -> not $ isSpace c || c `elem` "\\.#();"

symbol :: String -> Parser String
symbol = P.symbol lexer

name :: Parser Name
name = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

expr :: Parser Expr
expr = App <$> subExpr <*> many subExpr

subExpr :: Parser Expr
subExpr = lam
      <|> let_
      <|> charLit
      <|> strLit
      <|> parens
      <|> Var <$> name
  where
    lam     = Lam <$ symbol "\\" <*> many1 name <* symbol "." <*> expr
    let_    = Let <$ reserved "let" <*> decls <* reserved "in" <*> expr
    charLit = CharLit <$> P.charLiteral lexer
    strLit  = StrLit <$> P.stringLiteral lexer
    parens  = P.parens lexer expr

decls :: Parser [Decl]
decls = sepEndBy1 decl (P.semi lexer)

decl :: Parser Decl
decl = Decl <$> name <* reserved "=" <*> expr
