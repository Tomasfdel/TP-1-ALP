module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]
data Error = DivByZero | UndefVar deriving Show
type Trace = [String]

-- Estado nulo
initState :: State
initState = []

initTrace :: Trace
initTrace = []

-- Busca el valor de una variable en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Either Error Integer
lookfor var [] = Left UndefVar
lookfor var ((x, value):xs) = if var == x then Right value 
                                          else lookfor var xs

-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Integer -> State -> State
update var updateVal [] = [(var, updateVal)]
update var updateVal ((x, value):xs) = if var == x then (x, updateVal):xs 
                                                   else (x,value) : (update var updateVal xs)

handleUnaryExpression :: Either Error a -> (a -> a) -> Either Error a
handleUnaryExpression (Left error) f = Left error
handleUnaryExpression (Right val) f = Right (f val)

handleBinaryExpression :: Either Error a -> Either Error a -> (a -> a -> b) -> Either Error b
handleBinaryExpression (Left error1) _ _ = Left error1
handleBinaryExpression _ (Left error2) _ = Left error2
handleBinaryExpression (Right val1) (Right val2) f = Right (f val1 val2)


-- Evalua un programa en el estado nulo
eval :: Comm -> (Either Error State, Trace)
eval p = evalComm p initState initTrace

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> Trace -> (Either Error State, Trace)
evalComm Skip state trace = (Right state, trace)
evalComm (Let var intE) state trace = case (evalIntExp intE state) of
                                           Left error -> (Left error, trace)
                                           Right value -> (Right (update var value state), trace ++ ["Let " ++ var ++ " " ++ show(value)])
evalComm (Seq comm1 comm2) state trace = case (evalComm comm1 state trace) of
                                              (Left error, trace2) -> (Left error, trace2)
                                              (Right state2, trace2) -> evalComm comm2 state2 trace2
evalComm (Cond boolE commT commF) state trace = case (evalBoolExp boolE state) of
                                                     Left error -> (Left error, trace)
                                                     Right True -> evalComm commT state trace
                                                     Right False -> evalComm commF state trace
evalComm (Repeat comm cond) state trace = evalComm (Seq comm (Cond cond Skip (Repeat comm cond))) state trace


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const int) state = Right int
evalIntExp (Var variable) state = lookfor variable state
evalIntExp (UMinus intE) state = handleUnaryExpression (evalIntExp intE state) (\x -> -x)
evalIntExp (Plus intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x + y)
evalIntExp (Minus intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x - y)
evalIntExp (Times intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x * y)
evalIntExp (Div intE1 intE2) state = case (evalIntExp intE2 state) of
                                          Right 0 -> Left DivByZero
                                          x -> handleBinaryExpression (evalIntExp intE1 state) x (\x y -> div x y)


-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue state = Right True
evalBoolExp BFalse state = Right False
evalBoolExp (Eq intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x == y)
evalBoolExp (Lt intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x < y)
evalBoolExp (Gt intE1 intE2) state = handleBinaryExpression (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x > y)
evalBoolExp (And boolE1 boolE2) state = handleBinaryExpression (evalBoolExp boolE1 state) (evalBoolExp boolE2 state) (\x y -> x && y)
evalBoolExp (Or boolE1 boolE2) state = handleBinaryExpression (evalBoolExp boolE1 state) (evalBoolExp boolE2 state) (\x y -> x || y)
evalBoolExp (Not boolE) state = handleUnaryExpression (evalBoolExp boolE state) (\x -> not x)

