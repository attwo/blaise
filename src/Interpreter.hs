{-# LANGUAGE BangPatterns #-}
module Interpreter(interpretate) where

import           Prelude hiding(repeat)
import           Language
import           Control.Monad(liftM, forM_, when, void)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Trans.State.Strict hiding (state)
import           Control.Applicative((<*>), (<$>))
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import           Data.Maybe(maybeToList)


interpretate :: Program -> IO ()
interpretate (Program _ vars procs stmts) = do
    let state = initProgramState vars
        procsMap = initProcsMap procs
    evalStateT (mapM_ (interprateStatement procsMap) stmts) state

type Scope = Map Identifier Integer

initScope :: [Identifier] -> Scope
initScope = foldl (\m v -> M.insert v 0 m) M.empty

-- Глобальная облась видимости и локальная область видимости
type ProgramState = (Scope, Scope)

initProgramState :: [Identifier] -> ProgramState
initProgramState vars = (initScope vars, M.empty)

type ProgramMonad a = StateT ProgramState IO a

setLocalScope :: Scope -> ProgramMonad Scope
setLocalScope s = do
    !(g,l) <- get
    put (g,s)
    return l

type Procedures = Map (Identifier, Int) Procedure

initProcsMap :: [Procedure] -> Procedures
initProcsMap = foldl (\m p -> M.insert (procedureName p, arity p) p m) M.empty


checkDefined :: (Ord k, Show k) => String -> k -> Map k v -> v
checkDefined role k m  = 
    M.findWithDefault (error $ role ++ " " ++ show k ++ " is undefined") k m

checkedVariable :: Identifier -> ProgramMonad Integer
checkedVariable var = gets (\(g,l) ->
    let localValue = maybeToList $ M.lookup var l
        globalValue = maybeToList $ M.lookup var g
        !value = 
            if null localValue && null globalValue then
                error $ "Variable " ++ show var ++ " is undefined"
            else 
                head $ localValue ++ globalValue
    in value)

setVariable :: Identifier -> Integer -> ProgramMonad ()
setVariable var newValue = modify (\(g,l) ->
    if M.member var l then 
        (g, M.insert var newValue l)
    else
        (M.insert var newValue g, l))

checkedProcedure :: Procedures -> Identifier -> Int -> Procedure
checkedProcedure procs name arity =
    let !proc = checkDefined "Function" (name, arity) procs 
    in proc

checkedFunction :: Procedures -> Identifier -> Int -> Procedure
checkedFunction procs name arity =
    let !func = checkDefined "Function" (name, arity) procs 
    in if isProcedure func then
        error $ name ++ "(" ++ show arity ++ ") is not a function"
    else
        func
            


arithmeticOperation :: ABinOp -> Integer -> Integer -> Integer
arithmeticOperation op =
    case op of
        Add      -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide   -> (\v1 v2 ->
            if v2 == 0 then
                error "Division by 0"
            else 
                v1 `div` v2)

computeArithmeticExpression :: Procedures -> ArithmeticExpression -> ProgramMonad Integer
computeArithmeticExpression procs expr =
    case expr of
        Var v                  -> checkedVariable v
        Constant c             -> return $! c
        FunctionCall f args    -> functionCall procs f args
        Negated expr'          -> liftM negate $ computeArithmeticExpression procs expr'
        ABinary op expr1 expr2 -> 
              arithmeticOperation op
          <$> (computeArithmeticExpression procs expr1) 
          <*> (computeArithmeticExpression procs expr2)

relationalOperation :: RelationalBinaryOperator -> Integer -> Integer -> Bool
relationalOperation op = 
    case op of
        Greater -> (>)
        Less    -> (<)
        Equals  -> (==)

booleanOperation :: BooleanBinaryOperator -> Bool -> Bool -> Bool
booleanOperation op = 
    case op of
        And -> (&&)
        Or  -> (||)

computeBooleanExpression :: Procedures -> BooleanExpression -> ProgramMonad Bool
computeBooleanExpression procs expr =
    case expr of
        Not expr' -> liftM not $ computeBooleanExpression procs expr'
        BBinary op expr1 expr2 ->
              booleanOperation op
          <$> (computeBooleanExpression procs expr1)
          <*> (computeBooleanExpression procs expr2)
        RBinary op expr1 expr2 ->
              relationalOperation op
          <$> (computeArithmeticExpression procs expr1)
          <*> (computeArithmeticExpression procs expr2)


interprateStatement :: Procedures -> Statement -> ProgramMonad ()
interprateStatement procs stmt =
    case stmt of
      Assign var expr         -> assign procs var expr
      If cond stmts elseStmts -> ifelse procs cond stmts elseStmts
      While cond stmts        -> while procs cond stmts
      For var start end stmts -> for procs var start end stmts
      Repeat stmts cond       -> repeat procs stmts cond
      Call proc args          -> call procs proc args

            
assign :: Procedures -> String -> ArithmeticExpression -> ProgramMonad ()
assign procs var expr = do
    checkedVariable var
    !newValue <- computeArithmeticExpression procs expr
    setVariable var newValue

ifelse :: Procedures -> BooleanExpression -> [Statement] -> [Statement] -> ProgramMonad ()
ifelse procs cond ifStmts elseStmts = do
    !isTrue <- computeBooleanExpression procs cond
    mapM_ (interprateStatement procs)
          (if isTrue then ifStmts else elseStmts)

while :: Procedures -> BooleanExpression -> [Statement] -> ProgramMonad ()
while procs cond stmts = do
    !isTrue <- computeBooleanExpression procs cond
    when (isTrue) $ do
        mapM_ (interprateStatement procs) stmts
        while procs cond stmts

for :: Procedures -> Identifier -> Integer -> Integer -> [Statement] -> ProgramMonad ()
for procs var start end stmts = do
    checkedVariable var
    let sign = signum (end - start)
        second = start + sign
        list = if start == end then [start] else [start, second .. end]
    forM_ list $ (\v -> do
       setVariable var v
       mapM_ (interprateStatement procs) stmts
       )

repeat :: Procedures -> [Statement] -> BooleanExpression -> ProgramMonad ()
repeat procs stmts cond = do
    mapM_ (interprateStatement procs) stmts
    while procs cond stmts

call :: Procedures -> Identifier -> [ArithmeticExpression] -> ProgramMonad ()
call procs name args = do
-- if wreteln || readln then smth
    !computedArgs <- mapM (computeArithmeticExpression procs) args
    case name of
        "write"   | length args == 1 -> write $ head computedArgs
        "writeln" | length args == 1 -> writeln $ head computedArgs
        "read"    | length args == 1 && aIsVar (head args) 
                    -> readln $ aVarName $ head args
        "readln"  | length args == 1 && aIsVar (head args) 
                    -> readln $ aVarName $ head args
        _ -> call' procs name computedArgs

write :: Integer -> ProgramMonad ()
write = liftIO . putStr . show

writeln :: Integer -> ProgramMonad ()
writeln = liftIO . print

readln :: Identifier -> ProgramMonad ()
readln var = do
    checkedVariable var
    newVal <- liftIO readLn
    setVariable var newVal

call' :: Procedures -> Identifier -> [Integer] -> ProgramMonad ()
call' procs name computedArgs = do
    let !proc = checkedProcedure procs name (length computedArgs)
    let argsScope = M.fromList (zip (arguments proc) computedArgs)
        localVarsScope = initScope (variables proc)
        localScope = (if isFunction proc then M.insert name 0 else id) 
                   $ argsScope `M.union` localVarsScope
    oldLocalScope <- setLocalScope localScope
    mapM_ (interprateStatement procs) $ statements proc
    void $ setLocalScope oldLocalScope

functionCall :: Procedures -> Identifier -> [ArithmeticExpression] -> ProgramMonad Integer
functionCall procs name args = do
    let !func = checkedFunction procs name (length args)
    !computedArgs <- mapM (computeArithmeticExpression procs) args
    let argsScope = M.fromList (zip (arguments func) computedArgs)
        localVarsScope = initScope (variables func)
        localScope = M.insert name 0 $ argsScope `M.union` localVarsScope
    oldLocalScope <- setLocalScope localScope
    mapM_ (interprateStatement procs) $ statements func
    !result <- checkedVariable name
    setLocalScope oldLocalScope
    return $! result
