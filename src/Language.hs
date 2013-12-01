module Language where


data BooleanExpression = 
             Not BooleanExpression
           | BBinary BooleanBinaryOperator BooleanExpression BooleanExpression
           | RBinary RelationalBinaryOperator ArithmeticExpression ArithmeticExpression
    deriving (Show)

data BooleanBinaryOperator = And | Or 
    deriving (Show)

data RelationalBinaryOperator = Greater | Less | Equals 
    deriving (Show)

type Identifier = String

data ArithmeticExpression = 
             Var Identifier
           | Constant Integer
           | FunctionCall Identifier [ArithmeticExpression]
           | Negated ArithmeticExpression
           | ABinary ABinOp ArithmeticExpression ArithmeticExpression
    deriving (Show)

aIsVar :: ArithmeticExpression -> Bool
aIsVar (Var _) = True
aIsVar _       = False

aVarName :: ArithmeticExpression -> Identifier
aVarName (Var name) = name
aVarName _          = error "Is not var"

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
    deriving (Show)

data Statement = 
            Assign Identifier ArithmeticExpression
          | If BooleanExpression [Statement] [Statement]
          | While BooleanExpression [Statement]
          | For Identifier Integer Integer [Statement]
          | Repeat [Statement] BooleanExpression
          | Call Identifier [ArithmeticExpression]
    deriving (Show)


data VariableDeclaration = Vars [Identifier]
    deriving (Show)

data Procedure = 
    Procedure 
    { procedureName :: Identifier
    , arguments     :: [Identifier]
    , variables     :: [Identifier]
    , statements    :: [Statement]
    }
  | Function  
    { procedureName :: Identifier
    , arguments     :: [Identifier]
    , variables     :: [Identifier]
    , statements    :: [Statement]
    }
  deriving (Show)

isProcedure :: Procedure -> Bool
isProcedure Procedure{} = True
isProcedure _ = False

isFunction :: Procedure -> Bool
isFunction = not . isProcedure

arity :: Procedure -> Int
arity = length . arguments

data Program = Program Identifier [Identifier] [Procedure] [Statement]
    deriving (Show)

