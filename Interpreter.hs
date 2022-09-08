module Interpreter where

import qualified Data.Map as Map
import qualified Data.Stack as Stack

data ByteCode
  = LOAD_VAL Double
  | READ_VAR String
  | WRITE_VAR String
  | RETURN_VALUE
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  deriving (Show)

type Identifier = String

type Env = Map.Map Identifier Double

type Stack = Stack.Stack Double

type AppState = (Env, Stack)

-- Some constants to play around with. "f" takes the
-- place of some function expression, so for the
-- instruction RETURN_VALUE we write that returned
-- value to "f". Not ideal but it's something.
stackEmpty = Stack.stackNew

env3 = Map.fromList [("a", 3.0), ("b", 7.0), ("f", 0.0)]

-- Take a binary operator and pop 2 elements off the stack, 
-- apply the operator, then push the result on the stack.
-- If there are fewer than 2 elements on the stack then 
-- the error will propagate throughout the rest of the bytecode
-- instructions. 
applyBinOp :: (Double -> Double -> Double) -> AppState -> Either String AppState
applyBinOp binOp (env, stack) = case Stack.stackPop stack of
  Nothing -> Left "0 elements on stack, need 2 for binary operation"
  Just (stack', n) -> case Stack.stackPop stack' of
    Nothing -> Left "Only 1 element on stack, need 2 for binary operation"
    Just (stack'', k) -> Right (env, Stack.stackPush stack'' (binOp n k))

-- Take a list of bytecode instructions, and Either valid
-- AppState or an error from any previous invalid instructions
runByteCode :: [ByteCode] -> Either String AppState -> Either String AppState
runByteCode _ (Left str) = Left str
runByteCode [] (Right (env, stack)) = Right (env, stack)
runByteCode [LOAD_VAL val] (Right (env, stack))
 = Right (env, Stack.stackPush stack val)
runByteCode [READ_VAR var] (Right (env, stack)) = case Map.lookup var env of
  Nothing -> Left "Variable not found"
  Just val -> runByteCode [(LOAD_VAL val)] (Right (env, stack))
runByteCode [WRITE_VAR var] (Right (env, stack)) = case Stack.stackPop stack of
  Nothing -> Left "Empty stack. Need a value on the stack to write to a var"
  Just (stack', n) -> Right (Map.insert var n env, stack')
runByteCode [RETURN_VALUE] (Right (env, stack)) = case Stack.stackPop stack of
  Nothing -> Left "Empty stack. No value to return"
  Just (stack', n) -> runByteCode [WRITE_VAR "f"] (Right (env, stack))
runByteCode [ADD] (Right (env, stack)) = applyBinOp (+) (env, stack)
runByteCode [SUBTRACT] (Right (env, stack)) = applyBinOp (subtract) (env, stack)
runByteCode [MULTIPLY] (Right (env, stack)) = applyBinOp (*) (env, stack)
runByteCode [DIVIDE] (Right (env, stack)) = applyBinOp (/) (env, stack)
runByteCode (x : xs) (Right (env, stack)) =
  let output = runByteCode [x] (Right (env, stack))
   in runByteCode xs output

