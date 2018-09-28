module Eval2 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]
data Error = DivByZero | UndefVar deriving Show

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Integer
lookfor var [] = Left UndefVar
lookfor var ((x, value):xs) = if var == x 
                              then Right value 
                              else lookfor var xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update var updateVal [] = [(var, updateVal)]
update var updateVal ((x, value):xs) = 
    if var == x 
    then (x, updateVal):xs 
    else (x,value) : (update var updateVal xs)

handleUnExpr :: Either Error a -> (a -> a) -> Either Error a
handleUnExpr (Left error) f = Left error
handleUnExpr (Right val) f = Right (f val)

handleBinExpr :: Either Error a -> Either Error a -> (a -> a -> b) -> Either Error b
handleBinExpr (Left error1) _ _ = Left error1
handleBinExpr _ (Left error2) _ = Left error2
handleBinExpr (Right val1) (Right val2) f = Right (f val1 val2)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> Either Error State
evalComm Skip state = Right state
evalComm (Let var intE) state = case (evalIntExp intE state) of
                                     Left error -> Left error
                                     Right value -> Right (update var value state)
evalComm (Seq comm1 comm2) state = case (evalComm comm1 state) of
                                        Left error -> Left error
                                        Right state2 -> evalComm comm2 state2
evalComm (Cond boolE commT commF) state = case (evalBoolExp boolE state) of
                                               Left error -> Left error
                                               Right True -> evalComm commT state
                                               Right False -> evalComm commF state
evalComm (Repeat comm cond) state = 
    evalComm (Seq comm (Cond cond Skip (Repeat comm cond))) state


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Either Error Integer
evalIntExp (Const int) state = Right int
evalIntExp (Var variable) state = lookfor variable state
evalIntExp (UMinus intE) state = handleUnExpr (evalIntExp intE state) (\x -> -x)
evalIntExp (Plus intE1 intE2) state = 
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x + y)
evalIntExp (Minus intE1 intE2) state = 
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x - y)
evalIntExp (Times intE1 intE2) state =
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x * y)
evalIntExp (Div intE1 intE2) state =
     case (evalIntExp intE2 state) of
          Right 0 -> Left DivByZero
          x -> handleBinExpr (evalIntExp intE1 state) x (\x y -> div x y)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Either Error Bool
evalBoolExp BTrue state = Right True
evalBoolExp BFalse state = Right False
evalBoolExp (Eq intE1 intE2) state = 
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x == y)
evalBoolExp (Lt intE1 intE2) state = 
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x < y)
evalBoolExp (Gt intE1 intE2) state = 
    handleBinExpr (evalIntExp intE1 state) (evalIntExp intE2 state) (\x y -> x > y)
evalBoolExp (And boolE1 boolE2) state = 
    handleBinExpr (evalBoolExp boolE1 state) (evalBoolExp boolE2 state) (\x y -> x && y)
evalBoolExp (Or boolE1 boolE2) state = 
    handleBinExpr (evalBoolExp boolE1 state) (evalBoolExp boolE2 state) (\x y -> x || y)
evalBoolExp (Not boolE) state = handleUnExpr (evalBoolExp boolE state) (\x -> not x)
