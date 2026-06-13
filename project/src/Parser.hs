module Parser (parseContract) where

import AST

import Text.Parsec
import Text.Parsec.Expr     -- buildExpressionParser: handles operator precedence
import Text.Parsec.String (Parser)

skipSpacesAndComments :: Parser ()
skipSpacesAndComments = skipMany (skipMany1 space <|> lineComment)
  where
    lineComment = try (string "--") >> skipMany (satisfy (/= '\n'))

withTrailingWhitespace :: Parser a -> Parser a
withTrailingWhitespace p = do
  result <- p
  skipSpacesAndComments
  return result

symbol :: String -> Parser String
symbol s = withTrailingWhitespace (string s)

reserved :: [String]
reserved =
  [ "contract", "state", "transaction"
  , "require", "if", "else"
  , "sender", "true", "false", "empty", "not"
  , "int", "bool", "address", "map"
  ]

keyword :: String -> Parser ()
keyword w = withTrailingWhitespace (try (do
  _ <- string w
  notFollowedBy (alphaNum <|> char '_')))

identifier :: Parser String
identifier = withTrailingWhitespace (try (do
  first <- letter <|> char '_'
  rest  <- many (alphaNum <|> char '_')
  let name = first : rest
  if name `elem` reserved
    then unexpected ("reserved keyword '" ++ name ++ "'")
    else return name))

pType :: Parser Type
pType
  =   do { keyword "int";     return TInt     }
  <|> do { keyword "bool";    return TBool    }
  <|> do { keyword "address"; return TAddress }
  <|> pMapType

pMapType :: Parser Type
pMapType = do
  keyword "map"
  symbol "<"
  k <- pType
  symbol ","
  v <- pType
  symbol ">"
  return (TMap k v)

pExpr :: Parser Expr
pExpr = buildExpressionParser operatorTable pTerm

operatorTable =
  [ [ infixL "||"  Or  ]
  , [ infixL "&&"  And ]
  , [ infixL "==" Eq,  infixL "!=" Neq
    , infixL "<=" Lte, infixL ">=" Gte
    , infixL "<"  Lt,  infixL ">"  Gt
    ]
  , [ infixL "+" Add, infixL "-" Sub ]
  , [ infixL "*" Mul, infixL "/" Div ]
  , [ Prefix (do { keyword "not"; return Not }) ]
  ]
  where
    infixL sym op = Infix (do
      symbol sym
      return (\l r -> BinOp op l r))
      AssocLeft

pTerm :: Parser Expr
pTerm = do
  base <- pAtom
  keys <- many pIndexSuffix
  return (foldl Index base keys)

pIndexSuffix :: Parser Expr
pIndexSuffix = between (symbol "[") (symbol "]") pExpr

pAtom :: Parser Expr
pAtom
  =   do { keyword "sender"; return Sender              }
  <|> do { keyword "true";   return (Lit (VBool True))  }
  <|> do { keyword "false";  return (Lit (VBool False)) }
  <|> do { keyword "empty";  return (Lit (VMap []))     }
  <|> pInteger
  <|> pAddressLit
  <|> do { name <- identifier; return (Var name)        }
  <|> pParens

pInteger :: Parser Expr
pInteger = withTrailingWhitespace (do
  digits <- many1 digit
  return (Lit (VInt (read digits))))

pAddressLit :: Parser Expr
pAddressLit = withTrailingWhitespace (do
  _       <- char '"'
  content <- many (noneOf "\"")
  _       <- char '"'
  return (Lit (VAddress content)))

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pStatement :: Parser Statement
pStatement
  =   pRequire
  <|> pIf
  <|> pAssign

-- require expr ;
pRequire :: Parser Statement
pRequire = do
  keyword "require"
  e <- pExpr
  symbol ";"
  return (Require e)

pIf :: Parser Statement
pIf = do
  keyword "if"
  cond       <- pExpr
  thenBranch <- pBlock
  elseBranch <- option [] (do
    keyword "else"
    pBlock)
  return (If cond thenBranch elseBranch)

pAssign :: Parser Statement
pAssign = do
  lhs <- pExpr
  symbol ":="
  rhs <- pExpr
  symbol ";"
  return (Assign lhs rhs)

pBlock :: Parser [Statement]
pBlock = between (symbol "{") (symbol "}") (many pStatement)

pStateVar :: Parser StateVar
pStateVar = do
  name <- identifier
  symbol ":"
  t    <- pType
  symbol "="
  ini  <- pExpr
  symbol ";"
  return (StateVar name t ini)

pTransactionDef :: Parser TransactionDef
pTransactionDef = do
  keyword "transaction"
  name   <- identifier
  params <- between (symbol "(") (symbol ")") pParams
  body   <- pBlock
  return (TransactionDef name params body)

pParams :: Parser [(String, Type)]
pParams = sepBy pOneParam (symbol ",")
  where
    pOneParam = do
      name <- identifier
      symbol ":"
      t    <- pType
      return (name, t)

pContract :: Parser Contract
pContract = do
  skipSpacesAndComments
  keyword "contract"
  name      <- identifier
  symbol "{"
  stateVars <- option [] pStateBlock
  txDefs    <- many pTransactionDef
  symbol "}"
  eof
  return (Contract name stateVars txDefs)

pStateBlock :: Parser [StateVar]
pStateBlock = do
  keyword "state"
  between (symbol "{") (symbol "}") (many pStateVar)

parseContract :: String -> String -> Either ParseError Contract
parseContract = parse pContract