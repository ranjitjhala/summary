{-# LANGUAGE OverloadedStrings #-}

module Language.Summary.Parser ( parse, parseFile ) where

import           Control.Monad (void)
import           Text.Megaparsec hiding (parse)
import           Control.Monad.Combinators.Expr
-- import           Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char
import           Text.Megaparsec.Error 
import qualified Data.List as L
import qualified Data.Text as T
import           Language.Summary.Types
import           Data.List.NonEmpty         as NE
import           Language.Summary.Types
import           Language.Summary.UX

type Parser = Parsec SourcePos Text

--------------------------------------------------------------------------------
parseFile :: FilePath -> IO BareProgram
--------------------------------------------------------------------------------
parseFile f = parse f <$> readFile f

--------------------------------------------------------------------------------
parse :: FilePath -> Text -> BareProgram
----------------------------------------------------------------------------------
parse = parseWith prog

parseWith  :: Parser a -> FilePath -> Text -> a
parseWith p f s = case runParser (whole p) f s of
                    Left err -> panic (errorBundlePretty err) (errorPos err) -- (posSpan . NE.head . errorPos $ err)
                    Right e  -> e
                    -- Left err -> panic (show err) (posSpan . NE.head . errorPos $ err)

instance ShowErrorComponent SourcePos where 
  showErrorComponent = show 

errorPos :: ParseErrorBundle s e -> SourceSpan
errorPos =  posSpan . pstateSourcePos . bundlePosState

-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
-- --instance Located (ParseError a b) where
--   sourceSpan = posSpan . NE.head . errorPos

-- instance (Show a, Show b) => PPrint (ParseError a b) where
--   pprint = show

-- | Programs --------------------------------------------------------------------------------
prog :: Parser BareProgram
prog = Prog <$> many func <*> pure "main"

-- | Functions -------------------------------------------------------------------------------
func :: Parser BareFunc
func = withSpan' $ do
  rWord "def"
  f  <- ident
  xs <- parens (sepBy ident comma) <* colon
  s  <- stmt 
  return (Func f xs s)

-- | Identifiers -----------------------------------------------------------------------------
ident :: Parser Id 
ident = T.pack . fst <$> identifier 

-- | Statements ------------------------------------------------------------------------------
stmt :: Parser BareStmt 
stmt = undefined 

-- | `binder` parses BareBind, used for let-binds and function parameters.
-- binder :: Parser BareBind
-- binder = uncurry Bind <$> identifier


{- 

--------------------------------------------------------------------------------
-- | Top-Level Expression Parser
--------------------------------------------------------------------------------
expr :: Parser Bare
expr = makeExprParser expr0 binops

expr0 :: Parser Bare
expr0 =  try primExpr
     <|> try letExpr
     <|> try ifExpr
     <|> try getExpr
     <|> try appExpr
     <|> try tupExpr
     <|> try constExpr
     <|> idExpr

exprs :: Parser [Bare]
exprs = parens (sepBy1 expr comma)

getExpr :: Parser Bare
getExpr = withSpan' (GetItem <$> funExpr <*> brackets expr)

appExpr :: Parser Bare
appExpr = withSpan' (App <$> (fst <$> identifier) <*> exprs)

funExpr :: Parser Bare
funExpr = try idExpr <|> tupExpr

tupExpr :: Parser Bare
tupExpr = withSpan' (mkTuple <$> exprs)

mkTuple :: [Bare] -> SourceSpan -> Bare
mkTuple [e] _ = e
mkTuple es  l = Tuple es l

binops :: [[Operator Parser Bare]]
binops =
  [ [ InfixL (symbol "*"  *> pure (op Times))
    ]
  , [ InfixL (symbol "+"  *> pure (op Plus))
    , InfixL (symbol "-"  *> pure (op Minus))
    ]
  , [ InfixL (symbol "==" *> pure (op Equal))
    , InfixL (symbol ">"  *> pure (op Greater))
    , InfixL (symbol "<"  *> pure (op Less))
    ]
  ]
  where
    op o e1 e2 = Prim2 o e1 e2 (stretch [e1, e2])

idExpr :: Parser Bare
idExpr = uncurry Id <$> identifier

constExpr :: Parser Bare
constExpr
   =  (uncurry Number <$> integer)
  <|> (Boolean True   <$> rWord "true")
  <|> (Boolean False  <$> rWord "false")

primExpr :: Parser Bare
primExpr = withSpan' (Prim1 <$> primOp <*> parens expr)

primOp :: Parser Prim1
primOp
  =  try (rWord "add1"    *> pure Add1)
 <|> try (rWord "sub1"    *> pure Sub1)
 <|> try (rWord "isNum"   *> pure IsNum)
 <|> try (rWord "isBool"  *> pure IsBool)
 <|> try (rWord "isTuple" *> pure IsTuple)
 <|>     (rWord "print"  *> pure Print)

letExpr :: Parser Bare
letExpr = withSpan' $ do
  rWord "let"
  bs <- sepBy1 bind comma
  rWord "in"
  e  <- expr
  return (bindsExpr bs e)

bind :: Parser (BareBind, Bare)
bind = (,) <$> binder <* symbol "=" <*> expr

ifExpr :: Parser Bare
ifExpr = withSpan' $ do
  rWord "if"
  b  <- expr
  e1 <- between colon elsecolon expr
  e2 <- expr
  return (If b e1 e2)
  where
   elsecolon = rWord "else" *> colon

-- | `binder` parses BareBind, used for let-binds and function parameters.
binder :: Parser BareBind
binder = uncurry Bind <$> identifier


-}
--------------------------------------------------------------------------------
-- | Tokenisers and Whitespace
--------------------------------------------------------------------------------

-- | Top-level parsers (should consume all input)
whole :: Parser a -> Parser a
whole p = sc *> p <* eof

-- RJ: rename me "space consumer"
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- | `symbol s` parses just the string s (and trailing whitespace)
symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

-- | 'parens' parses something between parenthesis.
parens :: Parser a -> Parser a
parens = betweenS "(" ")"

-- | 'brackets' parses something between parenthesis.
brackets :: Parser a -> Parser a
brackets = betweenS "[" "]"

betweenS :: String -> String -> Parser a -> Parser a
betweenS l r = between (symbol l) (symbol r)

-- | `lexeme p` consume whitespace after running p
lexeme :: Parser a -> Parser (a, SourceSpan)
lexeme p = L.lexeme sc (withSpan p)

-- | 'integer' parses an integer.
integer :: Parser (Integer, SourceSpan)
integer = lexeme L.decimal

-- | `rWord`
rWord   :: String -> Parser SourceSpan
rWord w = snd <$> (withSpan (string w) <* notFollowedBy alphaNumChar <* sc)


-- | list of reserved words
keywords :: [Text]
keywords =
  [ "if"      , "else"
  , "true"    , "false"
  , "let"     , "in"
  , "add1"    , "sub1"  , "isNum"   , "isBool", "isTuple",  "print"
  , "def"     , "lambda"
  ]

withSpan' :: Parser (SourceSpan -> a) -> Parser a
withSpan' p = do
  p1 <- getSourcePos
  f  <- p
  p2 <- getSourcePos
  return (f (SS p1 p2))

withSpan :: Parser a -> Parser (a, SourceSpan)
withSpan p = do
  p1 <- getSourcePos
  x  <- p
  p2 <- getSourcePos
  return (x, SS p1 p2)

-- | `identifier` parses identifiers: lower-case alphabets followed by alphas or digits
identifier :: Parser (String, SourceSpan)
identifier = lexeme (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

stretch :: (Monoid a) => [Expr a] -> a
stretch = mconcat . fmap getLabel
