{-# LANGUAGE TupleSections #-}
module Jatko.Syntax where

import Control.Applicative
import Control.Arrow (first)
import Data.List
import Data.Maybe
import Data.Either
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style
import Control.Monad.Reader
import qualified Data.Map.Strict as M

import Jatko.Core
import Jatko.Syntax.Offside

data ParserEnv = ParserEnv
  { opTable :: OperatorTable Parser' (Expr Name)
  , constructors :: M.Map Name ()
  }

identifier :: (Monad m, TokenParsing m) => m Name
identifier = try (parens operator) <|> ident haskellIdents

operator :: (Monad m, TokenParsing m) => m Name
operator = ident emptyOps

parseWhole :: Parser Declarations
parseWhole = do
  (cons, table) <- runUnlined $ partitionEithers <$> sepBy (choice
    [ Left <$> (symbol "constructor" *> identifier)
    , Right <$> optional parseOperator]) newline

  runOffside $ runReaderT
    (do
      decs <- many (parseDec <* some newline)
      eof
      return $! foldl' (flip id) emptyDeclarations decs)
    ParserEnv
      { constructors = M.fromList $ map (flip (,) ()) cons
      , opTable = M.elems . M.fromListWith (++) . map (fmap pure) . catMaybes
        $ table }

parseDec :: Parser' (Declarations -> Declarations)
parseDec = choice
  [ do
    _ <- symbol "register"
    regName <- identifier
    (con, vs) <- parens ((,) <$> identifier <*> many identifier)
      <|> (,[]) <$> identifier
    symbol "="
    e <- expr
    return $ \ds -> ds
      { decRegistries = M.insertWith (++) regName [(con, vs, e)]
        $ decRegistries ds }
  , do
    _ <- symbol "constructor"
    con <- identifier
    symbol ":"
    e <- expr
    let go (Con "->" [arg, res]) = let (qs, vs, r) = go res in (qs, arg : vs, r)
        go (Forall v t) = let (qs, vs, r) = go t in (v : qs, vs, r)
        go t = ([], [], t)
    return $ \ds -> ds
      { decConstructors = M.insert con (go e) $ decConstructors ds }
  , do
    name <- identifier
    _ <- symbol "="
    e <- exprSig
    return $ \ds -> ds { decVars = M.insert name e $ decVars ds }

  ]

type Parser' = ReaderT ParserEnv (Offside Parser)

term :: Parser' (Expr Name)
term = token $ choice
  [ parens exprSig
  , do
    _ <- symbol "case"
    e <- expr
    _ <- symbol "of"
    skipMany newline
    clauses <- mapReaderT withIndent $ many ((,,) <$> identifier
      <*> many identifier
      <*> (symbol "->" *> expr))
    return $ Case clauses :$ e
  , do
    _ <- symbol "forall"
    v <- identifier
    _ <- symbol "."
    e <- expr
    return $ Forall v e
  , Lit <$> lit
  , do
    name <- identifier
    env <- ask
    return $ if M.member name (constructors env)
      then Con name []
      else Var name
  , do
    _ <- symbol "\\"
    vs <- some identifier
    _ <- symbol "->"
    a <- expr
    return $ foldr Lam a vs
  ]

exprSig :: Parser' (Expr Name)
exprSig = expr <**> ((symbol ":" *> fmap (flip (:::)) expr) <|> pure id)

expr :: Parser' (Expr Name)
expr = do
      table <- asks opTable
      buildExpressionParser table termA

termA :: Parser' (Expr Name)
termA = foldl (:$) <$> term <*> many (try term)

lit :: Parser' Literal
lit = choice
  [ either (LInt . fromInteger) LDbl <$> integerOrDouble
  , LStr <$> stringLiteral
  , fmap LJavaScript $ string "[|" *> manyTill anyChar (string "|]")
  ]

parseOperator :: Unlined Parser (Int, Operator Parser' (Expr Name))
parseOperator = do
  assoc <- parseAssoc
  i <- natural <?> "precedence"
  spaces
  name <- operator
  return (fromEnum i
    , Infix (do
      env <- ask
      op <- symbol name
      return $ \x y -> if M.member op (constructors env)
        then Con op [x, y]
        else Var op :$ x :$ y) assoc)
  where
    parseAssoc = choice
      [ AssocLeft <$ symbol "infixl"
      , AssocRight <$ symbol "infixr"
      , AssocNone <$ symbol "infix" ]
