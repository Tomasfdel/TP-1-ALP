module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM
 
instance Applicative State where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)

-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip            = return ()
evalComm (Let var ie)    = do n <- evalIntExp ie 
                              update var n
evalComm (Seq c1 c2)     = do evalComm c1 
                              evalComm c2
evalComm (Cond be c1 c2) = do b <- evalBoolExp be 
                              if b then (evalComm c1)
                                   else (evalComm c2)
evalComm (While be c)    = evalComm (Cond be (Seq c (While be c)) Skip)


-- Resuelve la evaluacion de dos expresiones enteras, aplicadas a un operador.
evalIntOp :: MonadState m => IntExp -> IntExp -> (Int -> Int -> a) -> m a
evalIntOp ie1 ie2 operator = do n1 <- evalIntExp ie1
                                n2 <- evalIntExp ie2
                                return (operator n1 n2)


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const i)       = return i
evalIntExp (Var x)         = lookfor x
evalIntExp (UMinus ie)     = do n <- evalIntExp ie 
                                return (-n)
evalIntExp (Plus ie1 ie2)  = evalIntOp ie1 ie2 (+)
evalIntExp (Minus ie1 ie2) = evalIntOp ie1 ie2 (-)
evalIntExp (Times ie1 ie2) = evalIntOp ie1 ie2 (*)
evalIntExp (Div ie1 ie2)   = evalIntOp ie1 ie2 div

                             
-- Resuelve la evaluacion de dos expresiones booleanas, aplicadas a un operador.
evalBoolOp :: MonadState m => BoolExp -> BoolExp -> (Bool -> Bool -> a) -> m a
evalBoolOp be1 be2 operator = do n1 <- evalBoolExp be1
                                 n2 <- evalBoolExp be2
                                 return (operator n1 n2)
                                 
                                 
-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BTrue         = return True
evalBoolExp BFalse        = return False
evalBoolExp (Eq ie1 ie2)  = evalIntOp ie1 ie2 (==)
evalBoolExp (Lt ie1 ie2)  = evalIntOp ie1 ie2 (<)
evalBoolExp (Gt ie1 ie2)  = evalIntOp ie1 ie2 (>)
evalBoolExp (And be1 be2) = evalBoolOp be1 be2 (&&)
evalBoolExp (Or be1 be2)  = evalBoolOp be1 be2 (||)
evalBoolExp (Not be)      = do b <- evalBoolExp be
                               return (not b)
