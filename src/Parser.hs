module Parser where

import           Language hiding (statements)

import           Control.Monad
import           Data.Either(partitionEithers)
import           Data.Maybe(maybeToList)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

pascalDef = emptyDef
    { Token.commentStart = "{"
    , Token.commentEnd   = "}"
    , Token.commentLine  = ""
    , Token.identStart   = letter <|> char '_'
    , Token.identLetter  = alphaNum <|> char '_'
    , Token.reservedNames = 
        [ "program", "function", "procedure", "var"
        , "begin", "end", "if", "then", "else"
        , "while", "repeat", "until", "for", "to", "downto", "do"
        ]
    , Token.reservedOpNames = 
        [ ":=", "=", "<", ">"
        , "and", "or", "not"
        , "+", "-", "*", "/"
        ]
    }

lexer = Token.makeTokenParser pascalDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
natural    = Token.natural    lexer -- parses a natural
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
dot        = Token.dot        lexer -- parses a dot
whiteSpace = Token.whiteSpace lexer -- parses whitespace
semiSep    = Token.semiSep    lexer -- semicolon separated list
semiSep1   = Token.semiSep1   lexer -- semicolon separated list
commaSep   = Token.commaSep   lexer -- comma separated list
commaSep1  = Token.commaSep1  lexer -- comma separated list


arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression = buildExpressionParser arithmeticOperators arithmeticTerm
 
booleanExpression :: Parser BooleanExpression
booleanExpression = buildExpressionParser booleanOperators booleanTerm

arithmeticOperators = 
     [ [Prefix (reservedOp "-"   >> return (Negated         ))          ]
     , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft]
     , [Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
     , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft]
     , [Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
     ]
 
booleanOperators = 
     [ [Prefix (reservedOp "not" >> return (Not             ))          ]
     , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft]
     , [Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
     ]

arithmeticTerm = 
      parens arithmeticExpression
  <|> try (liftM2 FunctionCall identifier (parens $ commaSep arithmeticExpression))
  <|> liftM Var identifier
  <|> liftM Constant integer 

booleanTerm = 
      parens booleanExpression
  <|> relationalExpression

relationalExpression =
  do a1 <- arithmeticExpression
     op <- relation
     a2 <- arithmeticExpression
     return $ RBinary op a1 a2
 
relation = (reservedOp ">" >> return Greater)
       <|> (reservedOp "<" >> return Less)
       <|> (reservedOp "=" >> return Equals)

statement :: Parser Statement
statement = 
      ifStatement 
  <|> whileStatement
  <|> forStatement
  <|> repeatStatement
  <|> try callStatement
  <|> assignStatement

ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    cond <- booleanExpression
    reserved "then"
    thenBody <- statementOrStatements
    elseBody <- optionMaybe (do { reserved "else"; statementOrStatements })
    return $! If cond thenBody (concat $ maybeToList $ elseBody)

whileStatement :: Parser Statement
whileStatement = do
    reserved "while"
    cond <- booleanExpression
    reserved "do"
    body <- statementOrStatements
    return $! While cond body

forStatement :: Parser Statement
forStatement = do
    reserved "for"
    var <- identifier
    reservedOp ":="
    startValue <- integer
    reserved "to" <|> reserved "downto"
    endValue <- integer
    reserved "do"
    body <- statementOrStatements
    return $! For var startValue endValue body

repeatStatement :: Parser Statement
repeatStatement = do
    reserved "repeat"
    body <- statementOrStatements
    reserved "until"
    cond <- booleanExpression
    return $! Repeat body cond

callStatement :: Parser Statement
callStatement = do
    name <- identifier
    args <- parens (commaSep arithmeticExpression)
    return $! Call name args
    
assignStatement :: Parser Statement
assignStatement = do
    name <- identifier
    reservedOp ":="
    value <- arithmeticExpression
    return $! Assign name value 

statements :: Parser [Statement]
statements = do
    stmts <- sepEndBy1 statement (semi)
    return $! stmts

statementsBlock :: Parser [Statement]
statementsBlock = do
    reserved "begin"
    list <- statements
    reserved "end"
    return $! list

statementOrStatements :: Parser [Statement]
statementOrStatements = 
    try statementsBlock <|> liftM (return :: a -> [a]) statement


allVars :: [VariableDeclaration] -> [String]
allVars = concatMap (\(Vars l) -> l)

variable :: Parser VariableDeclaration
variable = do
    reserved "var"
    whiteSpace
    l <- commaSep1 identifier
    return $! Vars l

callable :: Parser Procedure
callable = do
    try function <|> procedure

function :: Parser Procedure
function = do
    reserved "function"
    (name, params, vars, bodyStatement) <- callableCommon
    return $! Function name params vars bodyStatement

procedure :: Parser Procedure
procedure = do
    reserved "procedure"
    (name, params, vars, bodyStatement) <- callableCommon
    return $! Procedure name params vars bodyStatement

callableCommon :: Parser (String, [String], [Identifier], [Statement])
callableCommon = do
    name <- identifier
    params <- parens (commaSep identifier)
    semi
    vars <- sepEndBy variable semi
    reserved "begin"
    body <- statements
    reserved "end"
    return $! (name, params, allVars vars, body)

variableOrCallable :: Parser (Either VariableDeclaration Procedure)
variableOrCallable = 
      liftM Left variable
  <|> liftM Right callable

variablesOrCallables :: Parser [Either VariableDeclaration Procedure]
variablesOrCallables = do
    list <- sepEndBy variableOrCallable semi
    return $! list

program :: Parser Program
program = do
    reserved "program"
    name <- identifier
    semi
    eitherProcOrVar <- sepEndBy variableOrCallable semi
    bodyStatement <- statementsBlock
    dot
    let (varsDecls, procs) = partitionEithers eitherProcOrVar
        vars = allVars varsDecls
    return $! Program name vars procs bodyStatement
