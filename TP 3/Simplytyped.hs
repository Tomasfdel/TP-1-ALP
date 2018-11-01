module Simplytyped (
       conversion,    -- conversion a terminos localmente sin nombre
       eval,          -- evaluador
       infer,         -- inferidor de tipos
       quote          -- valores -> terminos
       )
       where

import Data.List
import Data.Maybe
import Prelude hiding ((>>=))
import Text.PrettyPrint.HughesPJ (render)
import PrettyPrinter
import Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LVar n)     = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (App t u)    = conversion' b t :@: conversion' b u
conversion' b (Abs n t u)  = Lam t (conversion' (n:b) u)
conversion' b (LLet var lt1 lt2) = Let (conversion' b lt1) (conversion' (var:b) lt2)
conversion' b (LAs lt typ) = As (conversion' b lt) typ
conversion' b LUnit = UnitT
conversion' b (LTuple lt1 lt2) = TupleT (conversion' b lt1) (conversion' b lt2)
conversion' b (LFirst lt) = First (conversion' b lt) 
conversion' b (LSecond lt) = Second (conversion' b lt)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n)              = Free n
sub i t (u :@: v)             = sub i t u :@: sub i t v
sub i t (Lam t' u)            = Lam t' (sub (i+1) t u)
sub i t (Let lt1 lt2)         = Let (sub i t lt1) (sub (i+1) t lt2)
sub i t (As lt typ)           = As (sub i t lt) typ
sub i t UnitT                 = UnitT
sub i t (TupleT lt1 lt2)      = TupleT (sub i t lt1) (sub i t lt2)
sub i t (First lt)            = First (sub i t lt)
sub i t (Second lt)           = Second (sub i t lt)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)                 = error "variable ligada inesperada en eval"
eval e (Free n)                  = fst $ fromJust $ lookup n e
eval _ (Lam t u)                 = VLam t u
eval e (Lam _ u :@: Lam s v)     = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)           = case eval e v of
                 VLam t' u' -> eval e (Lam t u :@: Lam t' u')
                 _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u :@: v)                 = case eval e u of
                 VLam t u' -> eval e (Lam t u' :@: v)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let lt1 lt2)             = case (eval e lt1) of
                 VLam t u' -> eval e (sub 0 (Lam t u') lt2)
                 _         -> error "Error de tipo en run-time, verificar type checker"
eval e (As lt typ)               = eval e lt
eval e UnitT                     = VUnit
eval e (TupleT lt1 lt2)          = VTuple (eval e lt1) (eval e lt2)
eval e (First (TupleT lt1 lt2))  = eval e lt1
eval e (First _ )                = error "Error de tipo de run-time, verificar type checker"
eval e (Second (TupleT lt1 lt2)) = eval e lt2
eval e (Second _ )               = error "Error de tipo de run-time, verificar type checker"

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=) :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 = err $ "se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

tupleError :: Either String Type
tupleError = err "Función de tupla aplicado a algo que no es una tupla"

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free n) = case lookup n e of
                        Nothing -> notfoundError n
                        Just (_,t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> 
                       infer' c e u >>= \tu ->
                       case tt of
                         Fun t1 t2 -> if (tu == t1) 
                                        then ret t2
                                        else matchError t1 tu
                         _         -> notfunError tt
infer' c e (Lam t u) = infer' (t:c) e u >>= \tu ->
                       ret $ Fun t tu
infer' c e (Let lt1 lt2) = infer' c e lt1 >>= (\tlt1 -> infer' (tlt1 : c) e lt2)
infer' c e (As lt typ) = infer' c e lt >>= (\tlt -> if tlt == typ then ret typ else matchError typ tlt)
infer' c e UnitT = ret Unit
infer' c e (TupleT lt1 lt2) = infer' c e lt1 >>= 
                                (\tlt1 -> infer' c e lt2 >>=
                                  (\tlt2 -> ret (Tuple tlt1 tlt2)))
infer' c e (First lt) = case infer' c e lt of
                          Right (Tuple t1 t2) -> ret t1
                          _ -> tupleError
infer' c e (Second lt) = case infer' c e lt of
                           Right (Tuple t1 t2) -> ret t2
                           _ -> tupleError                          
----------------------------------
