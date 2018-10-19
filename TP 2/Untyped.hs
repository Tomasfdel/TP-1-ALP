module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccion 2 - Representacion de Terminos Lambda 
-- Ejercicio 2: Conversion de Terminos
----------------------------------------------
           
lookFor :: String -> [String] -> Int -> Int
lookFor _ [] _ = -1
lookFor s (x:xs) dist = if s == x then dist else lookFor s xs (dist + 1)
           
convAux :: LamTerm -> [String] -> Term
convAux (LVar s) scope = case lookFor s scope 0 of
                            -1 -> Free s
                            n -> Bound n
convAux (App lt1 lt2) scope = (convAux lt1 scope) :@: (convAux lt2 scope)
convAux (Abs var lt) scope = Lam (convAux lt (var:scope))


conversion  :: LamTerm -> Term
conversion lt = convAux lt []

  
-------------------------------
-- Seccion 3 - Evaluacion
-------------------------------

shiftAux :: Term -> Int -> Int -> Term
shiftAux (Free s) _ _ = Free s
shiftAux (Bound n) c d = if n < c
                        then Bound n
                        else Bound (n + d)
shiftAux (lt1 :@: lt2) c d = (shiftAux lt1 c d) :@: (shiftAux lt2 c d)
shiftAux (Lam lt) c d = Lam (shiftAux lt (c + 1) d)

shift :: Term -> Int -> Term
shift lt d = shiftAux lt 0 d 
  
  
subst :: Term -> Term -> Int -> Term
subst (Free s) _ _  = Free s
subst x@(Bound n) lt' i = if n == i 
                           then lt'
                           else x
subst (lt1 :@: lt2) lt' i = (subst lt1 lt' i) :@: (subst lt2 lt' i)
subst (Lam lt) lt' i = Lam (subst lt (shift lt' 1) (i + 1)) 
                                   
betaRed :: Term -> Term -> Term
betaRed lt1 lt2 = shift (subst lt1 (shift lt2 1) 0) (-1)

lookInEnv :: Name -> NameEnv Term -> Either Term Term
lookInEnv s [] = Left (Free s)
lookInEnv s ((name, term):xs) = if s == name
                                   then (Right term)
                                   else lookInEnv s xs                                        
eval :: NameEnv Term -> Term -> Term
-- ~ Una variable libre evalua a si misma si no se encuentra en el 
-- ~ entorno, y a la evaluacion de su valor en caso contrario.
eval env (Free s) = case (lookInEnv s env) of
                         Left ft -> ft
                         Right t -> eval env t
-- ~ Una variable ligada evalua a si misma.                         
eval env (Bound n) = Bound n
-- ~ La evaluacion de una aplicacion de una variable libre a otro termino
-- ~ depende de si la variable se encuentra en el entorno.
eval env ((Free s) :@: lt2) = case (lookInEnv s env) of
                                   Left ft -> ft :@: (eval env lt2)
                                   Right t -> eval env (t :@: lt2)
-- ~ La aplicacion de una variable ligada a otro termino evalua a la
-- ~ aplicacion de la misma, a la evaluacion del termino.
eval env ((Bound n) :@: lt2) = (Bound n) :@: (eval env lt2)
-- ~ La aplicacion de una abstraccion a un termino evalua a la beta-reduccion,
-- ~ segun la regla E - APPABS.  
eval env ((Lam lt1) :@: lt2) = eval env (betaRed lt1 lt2)
-- ~ En caso de que el primer termino de la aplicacion no corresponda a los
-- ~ casos anteriores, se lo debe evaluar. En funcion de si corresponde a una
-- ~ abstraccion u otro termino se aplicara E-APPABS o E-APP2 respectivamente.
eval env (lt1 :@: lt2) = case (eval env lt1) of
                              (Lam lt) -> eval env ((Lam lt) :@: lt2)
                              lt -> lt :@: (eval env lt2)
 -- ~ La evaluacion de una abstraccion sera la abstraccion de la evaluacion del
-- ~ termino interno, segun la regla E - ABS.
eval env (Lam lt) = Lam (eval env lt)
