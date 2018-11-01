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
conversion' b  LUnit = UnitT
conversion' b (LTuple lt1 lt2) = TupleT (conversion' b lt1) (conversion' b lt2)
conversion' b (LFirst lt) = First (conversion' b lt) 
conversion' b (LSecond lt) = Second (conversion' b lt)
conversion' b  LZero = ZeroT
conversion' b (LSucc lt) = SuccT (conversion' b lt)
conversion' b (LR lt1 lt2 lt3) = RT (conversion' b lt1) (conversion' b lt2) (conversion' b lt3)

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
sub i t  UnitT                = UnitT
sub i t (TupleT lt1 lt2)      = TupleT (sub i t lt1) (sub i t lt2)
sub i t (First lt)            = First (sub i t lt)
sub i t (Second lt)           = Second (sub i t lt)
sub i t  ZeroT                = ZeroT
sub i t (SuccT lt)            = SuccT (sub i t lt)
sub i t (RT lt1 lt2 lt3)      = RT (sub i t lt1) (sub i t lt2) (sub i t lt3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _)                 = error "Variable ligada inesperada en eval"
eval e (Free n)                  = fst $ fromJust $ lookup n e
eval _ (Lam t u)                 = VLam t u
eval e (Lam _ u :@: Lam s v)     = eval e (sub 0 (Lam s v) u)
eval e (Lam t u :@: v)           = let value = eval e v
                                    in eval e (sub 0 (quote value) u)
eval e (Lam _ u :@: lt)          = eval e (sub 0 lt u)
eval e (u :@: v)                 = case eval e u of
                                     VLam t u' -> eval e (Lam t u' :@: v)
                                     _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let lt1 lt2)             = let value = eval e lt1
                                    in eval e (sub 0 (quote value) lt2)
eval e (As lt typ)               = eval e lt
eval e  UnitT                    = VUnit
eval e (TupleT lt1 lt2)          = VTuple (eval e lt1) (eval e lt2)
eval e (First (TupleT lt1 lt2))  = eval e lt1
eval e (First _ )                = error "Error de tipo de run-time, verificar type checker"
eval e (Second (TupleT lt1 lt2)) = eval e lt2
eval e (Second _ )               = error "Error de tipo de run-time, verificar type checker"
eval e  ZeroT                    = VNat Zero
eval e (SuccT lt)                = case eval e lt of
                                     VNat n -> VNat (Succ n)
                                     _      -> error "Error de tipo de run-time, verificar type checker"
eval e (RT lt1 lt2 lt3)          = case eval e lt3 of
                                     VNat Zero     -> eval e lt1
                                     VNat (Succ n) -> eval e ((lt2 :@: (quote (eval e (RT lt1 lt2 (quote (VNat n)))))) :@: (quote (VNat n)))

-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f)      = Lam t f
quote  VUnit          = UnitT
quote (VTuple v1 v2)  = TupleT (quote v1) (quote v2)
quote (VNat Zero)     = ZeroT
quote (VNat (Succ n)) = SuccT (quote (VNat n))

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
matchError t1 t2 = err $ "Se esperaba " ++
                         render (printType t1) ++
                         ", pero " ++
                         render (printType t2) ++
                         " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

tupleError :: Type -> Either String Type
tupleError t = err ("Función de tupla aplicado a un tipo " ++ render (printType t) ++ ".") 

succError :: Type -> Either String Type
succError t = err ("Sucesor aplicado a un tipo " ++ render (printType t) ++ ".")

rError1 :: Type -> Type -> Either String Type
rError1 t1 t2 = err ("Error de tipado en operador R. Tipo del primer argumento: " ++
                     render (printType t1) ++
                     ". Tipo del segundo argumento: " ++ 
                     render (printType t2) ++ ".")

rError2 :: Type -> Either String Type
rError2 t = err ("Operador R con tercer argumento de tipo " ++
                 render (printType t) ++
                 " cuando deberia ser Nat.")

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
infer' c e  UnitT = ret Unit
infer' c e (TupleT lt1 lt2) = infer' c e lt1 >>= 
                                (\tlt1 -> infer' c e lt2 >>=
                                  (\tlt2 -> ret (Tuple tlt1 tlt2)))
infer' c e (First lt) = infer' c e lt >>=
                          (\tlt -> case tlt of
                                     Right (Tuple t1 t2) -> ret t1
                                     Right t -> tupleError t)
infer' c e (Second lt) = infer' c e lt >>=
                           (\tlt -> case tlt of
                                      Right (Tuple t1 t2) -> ret t2
                                      Right t -> tupleError t)
infer' c e  ZeroT = ret Nat
infer' c e (SuccT lt) = infer' c e lt >>=
                          (\tlt -> case tlt of
                                     Right Nat -> Right Nat
                                     Right t -> succError t)
infer' c e (RT lt1 lt2 lt3) = infer' c e lt1 >>=
                                (\tlt1 -> infer' c e lt2 >>=
                                  (\tlt2 -> case tlt2 of
                                              Right (Fun t1A (Fun Nat t1B)) -> if t1A == tlt1 && t1B == tlt1 then infer' c e lt3 >>=
                                                                                                                    (\tlt3 -> case tlt3 of
                                                                                                                                Right Nat -> tlt1
                                                                                                                                Right t3  -> rError2 t3)
                                                                                                             else rError1 tlt1 (Fun t1A (Fun Nat t1B))
                                              Right t2 -> rError1 tlt1 t2))
----------------------------------
