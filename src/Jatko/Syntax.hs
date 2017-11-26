{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Jatko.Syntax where

import Control.Applicative
import Data.Char
import Data.List
import Data.Either
import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style
import Control.Monad.Reader
import qualified Data.Map.Strict as M

import Jatko.Core
import Jatko.Syntax.Offside
import Debug.Trace

data ParserEnv = ParserEnv
  { opTable :: OperatorTable Parser' (Expr Name)
  , constructors :: M.Map Name ()
  }

isConstructor :: Name -> Bool
isConstructor (c : cs) | isUpper c = True
isConstructor "->" = True
isConstructor _ = False

identifier :: (Monad m, TokenParsing m) => m Name
identifier = try (parens operator) <|> ident haskellIdents
  <?> "identifier"

operator :: (Monad m, TokenParsing m) => m Name
operator = ident emptyOps

parseWhole :: Parser Declarations
parseWhole = do
  (cons, table) <- runUnlined $ partitionEithers <$> sepBy (choice
    [ Left <$> (symbol "constructor" *> identifier)
    , Right <$> optional parseOperator]) newline

  let env = ParserEnv
        { constructors = M.fromList $ map (flip (,) ()) cons
        , opTable = M.elems $ M.fromListWith (++) [(p, [m]) | Just (p, _, _, m) <- table] }

  decs <- many $ runOffside 1 (runReaderT parseDec env) <* many newline
  eof
  return $! foldl' (flip id) emptyDeclarations
    { decOpTable = M.fromList [(n, (assoc, p)) | Just (p, n, assoc, _) <- table]} decs

parseDec :: Parser' (Declarations -> Declarations)
parseDec = choice
  [ do
    symbol "register"
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
  ] <?> "declaration"

type Parser' = ReaderT ParserEnv (Offside Parser)

parseCase :: Parser' (Expr Name)
parseCase = do
  _ <- symbol "case"
  e <- expr
  _ <- symbol "of"
  clauses <- mapReaderT withIndent $ many $ do
    con <- identifier
    vs <- many identifier
    symbol "->"
    e <- expr
    return (con, vs, e)
  return $ Case clauses :$ e

term :: Parser' (Expr Name)
term = token $ choice
  [ parens exprSig
  , parseDo
  , parseCase
  , do
    _ <- symbol "forall"
    vs <- some identifier
    _ <- symbol "."
    e <- expr
    return $ foldr Forall e vs
  , Lit <$> lit
  , do
    name <- identifier
    return $ if isConstructor name
      then Con name []
      else Var name
  , do
    _ <- symbol "\\"
    vs <- some identifier
    _ <- symbol "->"
    a <- expr
    return $ foldr Lam a vs
  ]

parseDo :: Parser' (Expr Name)
parseDo = do
  symbol "do"
  ss <- semiSep $ do
    v <- optional $ try $ identifier <* symbol "<-"
    m <- expr
    return (v, m)
    <?> "statement"
  traceShowM ss
  return $ Lam "#m" $ foldr
    (\(v, m) r -> Var "bind" :$ Var "#m" :$ m :$ Lam (maybe "_" id v) r)
    (snd (last ss)) (init ss)
  <?> "Do notation"

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

parseOperator :: Unlined Parser (Int, Name, Assoc, Operator Parser' (Expr Name))
parseOperator = do
  assoc <- parseAssoc
  i <- natural <?> "precedence"
  spaces
  name <- operator
  return (fromEnum i
    , name
    , assoc
    , Infix (do
      op <- symbol name
      return $ \x y -> if isConstructor name
        then Con op [x, y]
        else Var op :$ x :$ y) assoc)
  where
    parseAssoc = choice
      [ AssocLeft <$ symbol "infixl"
      , AssocRight <$ symbol "infixr"
      , AssocNone <$ symbol "infix" ]
