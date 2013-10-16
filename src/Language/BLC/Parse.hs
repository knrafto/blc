module Language.BLC.Parse
    ( Name
    , Decl(..)
    , Expr(..)
    , parseExpr
    , parseFile
    ) where

import           Control.Applicative    hiding (optional)
import           Control.Monad.Identity
import           Data.Char
import           Text.Parsec            hiding ((<|>), many)
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String
import qualified Text.Parsec.Token      as P

type Name = String

data Decl
    = Decl Name Expr
    | Import [String]
    deriving (Eq, Ord, Show, Read)

data Expr
    = Var Name
    | App Expr [Expr]
    | Lam [Name] Expr
    | Let [Decl] Expr
    | CharLit Char
    | StrLit String
    deriving (Eq, Ord, Show, Read)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr) ""

parseFile :: FilePath -> IO [Decl]
parseFile fp = do
    s <- readFile fp
    either (fail . show) return $ parse (whiteSpace *> decls) fp s

lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser emptyDef
    { P.commentLine   = "#"
    , P.identStart    = nameChar
    , P.identLetter   = nameChar
    , P.reservedNames = ["import", "let", "in", "="]
    }
  where
    nameChar = satisfy $ \c -> not $ isSpace c || c `elem` "\\.#();"

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

semi :: Parser String
semi = P.semi lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

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

lam :: Parser Expr
lam = Lam <$ symbol "\\" <*> many1 name <* symbol "." <*> expr

let_ :: Parser Expr
let_ = Let <$ reserved "let" <*> decls <* reserved "in" <*> expr

charLit :: Parser Expr
charLit = CharLit <$> P.charLiteral lexer

strLit :: Parser Expr
strLit = StrLit <$> P.stringLiteral lexer

parens :: Parser Expr
parens = P.parens lexer expr

decls :: Parser [Decl]
decls = sepEndBy1 decl semi

decl :: Parser Decl
decl = import_ <|> Decl <$> name <* reserved "=" <*> expr

import_ :: Parser Decl
import_ = Import <$ reserved "import" <*> lexeme path
  where
    path        = sepBy1 pathSegment (char '.')
    pathSegment = many1 pathChar
    pathChar    = satisfy $ \c -> not $ isSpace c || c `elem` "/."
