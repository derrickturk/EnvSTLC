{-# LANGUAGE DataKinds, TupleSections, OverloadedStrings #-}

module Language.EnvSTLC.Parser (
    Parser
  , ident
  , ty
  , term
  , stmt
  , replItem
  , parse
  , parseMaybe
  , parseTest
  , parseTest'
  , runParser
  , runParser'
  , parseErrorPretty
  , only
) where

import Language.EnvSTLC.Syntax
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

enclosed :: T.Text -> T.Text -> Parser a -> Parser a
enclosed left right = between (symbol left) (symbol right)

integer :: Parser Int
integer = lexeme $ L.signed space L.decimal

bool :: Parser Bool
bool = lexeme $ True <$ "true" <|> False <$ "false"

keywords :: [T.Text]
keywords = [ "if"
           , "then"
           , "else"
           , "true"
           , "false"
           , "let"
           , "in"
           ]

ident :: Parser Ident
ident = lexeme $ do
  var <- T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  if var `elem` keywords
    then failure
      (Just $ Label $ NE.fromList "keyword")
      (S.singleton $ Label $ NE.fromList "identifier")
    else return var

binOpRec :: (a -> b -> a) -> Parser a -> Parser b -> Parser a
binOpRec f base rest = foldl f <$> base <*> some rest

binOpsRec :: Parser (a -> b -> a) -> Parser a -> Parser b -> Parser a
binOpsRec op base rest =
  foldl (\b (o, r) -> o b r) <$> base <*> some ((,) <$> op <*> rest)

lowPrecOp :: Parser (Term s -> Term s -> Term s)
lowPrecOp =  Add <$ lexeme "+"
         <|> Sub <$ lexeme "-"
         <|> Or <$ lexeme "||"

highPrecOp :: Parser (Term s -> Term s -> Term s)
highPrecOp =  Mul <$ lexeme "*"
          <|> Div <$ lexeme "/"
          <|> And <$ lexeme "&&"

baseTy :: Parser Type
baseTy = lexeme $
      IntTy <$ "Int"
  <|> BoolTy <$ "Bool"
  <|> enclosed "(" ")" ty

ty :: Parser Type
ty = try (binOpRec (:->:) baseTy (lexeme "->" *> ty))
 <|> baseTy

term :: Parser (Term 'Unchecked)
term = lexeme $
      try lambda
  <|> try (binOpsRec lowPrecOp addend addend)
  <|> try (binOpRec App addend addend)
  <|> addend
  where
    lambda = Lam <$> (lexeme "\\" *> ident)
                 <*> (lexeme ":" *> ty)
                 <*> (lexeme "." *> term)

addend :: Parser (Term 'Unchecked)
addend =  try (binOpsRec highPrecOp atom atom)
      <|> atom

atom :: Parser (Term 'Unchecked)
atom = try (Var <$> ident)
   <|> try (IntLit <$> integer)
   <|> try (BoolLit <$> bool)
   <|> try (Not <$> (lexeme "!" *> atom))
   <|> try (enclosed "(" ")" term)
   <|> try (IfThenElse <$> (lexeme "if" *> space *> term)
                       <*> (lexeme "then" *> space *> term)
                       <*> (lexeme "else" *> space *> term))
   <|> (Let <$> (lexeme "let" *> space *> (stmt `sepBy1` semicolon))
            <*> (lexeme "in" *> space *> term))
  where
    semicolon = lexeme $ char ';'

stmt :: Parser (Stmt 'Unchecked)
stmt =  try (Declare <$> ident <*> (lexeme ":" *> ty))
    <|> (Define <$> ident <*> (lexeme "=" *> term))

replItem :: Parser ReplItem
replItem =  try (ReplStmt <$> stmt)
        <|> try (ReplTerm <$> term)
        <|> try (ReplCmd <$> (lexeme "?" *> ident))

only :: Parser a -> Parser a
only = (<* lexeme eof)
