module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Integer)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Integer
lookfor var ((x, value):xs) = if var == x 
                              then value 
                              else lookfor var xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Integer -> State -> State
update var updateVal [] = [(var, updateVal)]
update var updateVal ((x, value):xs) = 
    if var == x 
    then (x, updateVal):xs 
    else (x,value) : (update var updateVal xs)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm Skip state = state
evalComm (Let var intE) state = update var (evalIntExp intE state) state
evalComm (Seq comm1 comm2) state = let state2 = (evalComm comm1 state) 
                                   in evalComm comm2 state2
evalComm (Cond boolE commT commF) state = if (evalBoolExp boolE state) 
                                          then evalComm commT state
                                          else evalComm commF state
evalComm (Repeat comm cond) state = 
    evalComm (Seq comm (Cond cond Skip (Repeat comm cond))) state

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Integer
evalIntExp (Const int) state = int
evalIntExp (Var variable) state = lookfor variable state
evalIntExp (UMinus intE) state = -(evalIntExp intE state)
evalIntExp (Plus intE1 intE2) state = 
    (evalIntExp intE1 state) + (evalIntExp intE2 state)
evalIntExp (Minus intE1 intE2) state =
    (evalIntExp intE1 state) - (evalIntExp intE2 state)
evalIntExp (Times intE1 intE2) state = 
    (evalIntExp intE1 state) * (evalIntExp intE2 state)
evalIntExp (Div intE1 intE2) state = 
    div (evalIntExp intE1 state) (evalIntExp intE2 state)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp BTrue state = True
evalBoolExp BFalse state = False
evalBoolExp (Eq intE1 intE2) state = 
    (evalIntExp intE1 state) == (evalIntExp intE2 state)
evalBoolExp (Lt intE1 intE2) state = 
    (evalIntExp intE1 state) < (evalIntExp intE2 state)
evalBoolExp (Gt intE1 intE2) state =
    (evalIntExp intE1 state) > (evalIntExp intE2 state)
evalBoolExp (And boolE1 boolE2) state =
    (evalBoolExp boolE1 state) && (evalBoolExp boolE2 state)
evalBoolExp (Or boolE1 boolE2) state =
    (evalBoolExp boolE1 state) || (evalBoolExp boolE2 state)
evalBoolExp (Not boolE) state = not (evalBoolExp boolE state)
