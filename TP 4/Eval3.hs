module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)  

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado
newtype StateErrorTick a = StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Int, Env) }

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM
 
instance Applicative StateErrorTick where
    pure   = return
    (<*>)  = ap      

-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

class Monad m => MonadTick m where
    -- Crea un contador en 1
    tick :: m ()

instance Monad StateErrorTick where
    return x = StateErrorTick (\s -> Just (x, 0, s))
    m >>= f = StateErrorTick (\s -> case runStateErrorTick m s of
                                     Nothing -> Nothing
                                     Just (v, count, s') -> case runStateErrorTick (f v) s' of
                                                                Nothing -> Nothing
                                                                Just (v', count', s'') -> Just (v', count + count', s''))

instance MonadError StateErrorTick where
    throw = StateErrorTick (\s -> Nothing)

instance MonadState StateErrorTick where
    lookfor v = StateErrorTick (\s -> maybe (Nothing) (\u -> Just (u, 0, s)) (lookfor' v s))
                                      where lookfor' v [] = Nothing
                                            lookfor' v ((u,j):ss) | v == u    = Just j
                                                                  | otherwise = lookfor' v ss
    update v i = StateErrorTick (\s -> Just ((), 0, update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, j):ss) | v == u    = (v, i):ss
                                               | otherwise = (u, j):(update' v i ss)

instance MonadTick StateErrorTick where
    tick = StateErrorTick (\s -> Just ((), 1, s))

-- Evalua un programa en el estado nulo
eval :: Comm -> (Int, Env)
eval c = maybe ((-1), []) (\(v, count, s) -> (count, s)) (runStateErrorTick (evalComm c) initState)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
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
evalIntOp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> IntExp -> (Int -> Int -> a) -> m a
evalIntOp ie1 ie2 operator = do n1 <- evalIntExp ie1
                                n2 <- evalIntExp ie2
                                return (operator n1 n2)


--COMENTAR QUE SOLO CONTAMOS OPERACIONES BINARIAS
-- Evalua una expresion entera, sin efectos laterales.
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const i)       = return i
evalIntExp (Var x)         = lookfor x
evalIntExp (UMinus ie)     = do n <- evalIntExp ie 
                                return (-n)
evalIntExp (Plus ie1 ie2)  = tick >> evalIntOp ie1 ie2 (+)
evalIntExp (Minus ie1 ie2) = tick >> evalIntOp ie1 ie2 (-)
evalIntExp (Times ie1 ie2) = tick >> evalIntOp ie1 ie2 (*)
evalIntExp (Div ie1 ie2)   = do n1 <- evalIntExp ie1
                                n2 <- evalIntExp ie2
                                if n2 == 0 then throw
                                           else tick >> return (div n1 n2)

-- Resuelve la evaluacion de dos expresiones booleanas, aplicadas a un operador.
evalBoolOp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> BoolExp -> (Bool -> Bool -> a) -> m a
evalBoolOp be1 be2 operator = do n1 <- evalBoolExp be1
                                 n2 <- evalBoolExp be2
                                 return (operator n1 n2)
                                 
                                 
-- Evalua una expresion booleana, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
evalBoolExp BTrue         = return True
evalBoolExp BFalse        = return False
evalBoolExp (Eq ie1 ie2)  = evalIntOp ie1 ie2 (==)
evalBoolExp (Lt ie1 ie2)  = evalIntOp ie1 ie2 (<)
evalBoolExp (Gt ie1 ie2)  = evalIntOp ie1 ie2 (>)
evalBoolExp (And be1 be2) = evalBoolOp be1 be2 (&&)
evalBoolExp (Or be1 be2)  = evalBoolOp be1 be2 (||)
evalBoolExp (Not be)      = do b <- evalBoolExp be
                               return (not b)
