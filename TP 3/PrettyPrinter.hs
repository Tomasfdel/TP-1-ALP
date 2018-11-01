module PrettyPrinter (
       printTerm,     -- pretty printer para terminos
       printType,     -- pretty printer para tipos
       )
       where

import Common
import Text.PrettyPrint.HughesPJ

-- lista de posibles nombres para variables
vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Integer)..], c <- ['x','y','z'] ++ ['a'..'w'] ]
              
parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k)         = text (vs !! (ii - k - 1))
pp _  _  (Free (Global s)) = text s
pp ii vs (i :@: c) = sep [parensIf (isLam i || isLet i || isAs i) (pp ii vs i), 
                          nest 1 (parensIf (isLam c || isApp c || isLet c || isAs c) (pp ii vs c))]  
pp ii vs (Lam t c) = text "\\" <>
                     text (vs !! ii) <>
                     text ":" <>
                     printType t <>
                     text ". " <> 
                     pp (ii+1) vs c
pp ii vs (Let lt1 lt2) = text "let " <>
                         text (vs !! ii) <>
                         text " = " <>
                         parensIf (isLam lt1 || isApp lt1 || isLet lt1 || isAs lt1) (pp ii vs lt1) <>
                         text " in " <>
                         parensIf (isLam lt2 || isApp lt2 || isLet lt2 || isAs lt2) (pp (ii + 1) vs lt2)
pp ii vs (As lt typ) = parensIf (isApp lt || isLam lt || isLet lt || isAs lt) (pp ii vs lt) <>
                       text " as " <>
                       printType typ
pp ii vs UnitT = text "unit"
pp ii vs (TupleT lt1 lt2) = parens ((pp ii vs lt1) <>
                                   text ", " <>
                                   (pp ii vs lt2))
pp ii vs (First lt) = text "fst " <>
                      pp ii vs lt
pp ii vs (Second lt) = text "fst " <>
                       pp ii vs lt

isLam :: Term -> Bool                    
isLam (Lam _ _) = True
isLam  _      = False

isApp :: Term -> Bool        
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isAs :: Term -> Bool
isAs (As _ _) = True
isAs _        = False                                                               

-- pretty-printer de tipos
printType :: Type -> Doc
printType Base         = text "B"
printType (Fun t1 t2)  = sep [ parensIf (isFun t1) (printType t1), 
                               text "->", 
                               printType t2]
isFun :: Type -> Bool
isFun (Fun _ _)        = True
isFun _                = False

fv :: Term -> [String]
fv (Bound _)         = []
fv (Free (Global n)) = [n]
fv (t :@: u)         = fv t ++ fv u
fv (Lam _ u)         = fv u
fv (Let lt1 lt2)     = fv lt1 ++ fv lt2
fv (As lt typ)       = fv lt
fv UnitT             = []
fv (TupleT lt1 lt2)   = fv lt1 ++ fv lt2
fv (First lt)        = fv lt
fv (Second lt)       = fv lt
  
---
printTerm :: Term -> Doc 
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

