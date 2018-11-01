{-# OPTIONS_GHC -w #-}
module Parse where
import Common
import Data.Maybe
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t6 t7 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 (LamTerm)
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,104) ([16384,6353,0,16,5120,393,0,1,0,0,1024,0,8192,0,0,0,0,0,4,5120,393,0,0,4096,0,9296,6,35092,1,256,0,0,0,0,0,32,33792,16,0,0,32768,0,8192,16384,0,0,518,2048,0,0,0,64,0,37184,24,2112,1,0,0,25157,16384,6289,0,2,4096,66,0,0,0,0,640,0,16912,0,3072,0,514,8192,2,0,32,17664,98,0,0,9296,6,128,0,0,0,512,0,128,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseStmt","%start_parseStmts","%start_term","Def","Defexp","Exp","NAbs","Atom","Type","Defs","'='","':'","'\\\\'","'.'","'('","')'","','","'->'","VAR","TYPE","DEF","LET","IN","AS","UNIT","FST","SND","%eof"]
        bit_start = st * 30
        bit_end = (st + 1) * 30
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..29]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (23) = happyShift action_5
action_0 (24) = happyShift action_12
action_0 (28) = happyShift action_13
action_0 (29) = happyShift action_14
action_0 (6) = happyGoto action_17
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_18
action_0 (9) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (23) = happyShift action_5
action_1 (7) = happyGoto action_15
action_1 (12) = happyGoto action_16
action_1 _ = happyReduce_22

action_2 (15) = happyShift action_9
action_2 (17) = happyShift action_10
action_2 (21) = happyShift action_11
action_2 (24) = happyShift action_12
action_2 (28) = happyShift action_13
action_2 (29) = happyShift action_14
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 (10) = happyGoto action_8
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (23) = happyShift action_5
action_3 (7) = happyGoto action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (21) = happyShift action_27
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (26) = happyShift action_19
action_6 (30) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (17) = happyShift action_10
action_7 (21) = happyShift action_11
action_7 (28) = happyShift action_13
action_7 (29) = happyShift action_14
action_7 (10) = happyGoto action_26
action_7 _ = happyReduce_9

action_8 _ = happyReduce_11

action_9 (21) = happyShift action_25
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (15) = happyShift action_9
action_10 (17) = happyShift action_10
action_10 (21) = happyShift action_11
action_10 (24) = happyShift action_12
action_10 (28) = happyShift action_13
action_10 (29) = happyShift action_14
action_10 (8) = happyGoto action_24
action_10 (9) = happyGoto action_7
action_10 (10) = happyGoto action_8
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_12

action_12 (21) = happyShift action_23
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_9
action_13 (17) = happyShift action_10
action_13 (21) = happyShift action_11
action_13 (24) = happyShift action_12
action_13 (28) = happyShift action_13
action_13 (29) = happyShift action_14
action_13 (8) = happyGoto action_22
action_13 (9) = happyGoto action_7
action_13 (10) = happyGoto action_8
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_9
action_14 (17) = happyShift action_10
action_14 (21) = happyShift action_11
action_14 (24) = happyShift action_12
action_14 (28) = happyShift action_13
action_14 (29) = happyShift action_14
action_14 (8) = happyGoto action_21
action_14 (9) = happyGoto action_7
action_14 (10) = happyGoto action_8
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (23) = happyShift action_5
action_15 (7) = happyGoto action_15
action_15 (12) = happyGoto action_20
action_15 _ = happyReduce_22

action_16 (30) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (30) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (26) = happyShift action_19
action_18 _ = happyReduce_4

action_19 (17) = happyShift action_34
action_19 (22) = happyShift action_35
action_19 (27) = happyShift action_36
action_19 (11) = happyGoto action_33
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_21

action_21 (26) = happyShift action_19
action_21 _ = happyReduce_14

action_22 (26) = happyShift action_19
action_22 _ = happyReduce_13

action_23 (13) = happyShift action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (18) = happyShift action_30
action_24 (19) = happyShift action_31
action_24 (26) = happyShift action_19
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_29
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_10

action_27 (13) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (15) = happyShift action_9
action_28 (17) = happyShift action_10
action_28 (21) = happyShift action_11
action_28 (24) = happyShift action_12
action_28 (28) = happyShift action_13
action_28 (29) = happyShift action_14
action_28 (8) = happyGoto action_42
action_28 (9) = happyGoto action_7
action_28 (10) = happyGoto action_8
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (17) = happyShift action_34
action_29 (22) = happyShift action_35
action_29 (27) = happyShift action_36
action_29 (11) = happyGoto action_41
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_16

action_31 (15) = happyShift action_9
action_31 (17) = happyShift action_10
action_31 (21) = happyShift action_11
action_31 (24) = happyShift action_12
action_31 (28) = happyShift action_13
action_31 (29) = happyShift action_14
action_31 (8) = happyGoto action_40
action_31 (9) = happyGoto action_7
action_31 (10) = happyGoto action_8
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_9
action_32 (17) = happyShift action_10
action_32 (21) = happyShift action_11
action_32 (24) = happyShift action_12
action_32 (28) = happyShift action_13
action_32 (29) = happyShift action_14
action_32 (8) = happyGoto action_39
action_32 (9) = happyGoto action_7
action_32 (10) = happyGoto action_8
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (20) = happyShift action_38
action_33 _ = happyReduce_8

action_34 (17) = happyShift action_34
action_34 (22) = happyShift action_35
action_34 (27) = happyShift action_36
action_34 (11) = happyGoto action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_17

action_36 _ = happyReduce_19

action_37 (18) = happyShift action_47
action_37 (20) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (17) = happyShift action_34
action_38 (22) = happyShift action_35
action_38 (27) = happyShift action_36
action_38 (11) = happyGoto action_46
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (25) = happyShift action_45
action_39 (26) = happyShift action_19
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (18) = happyShift action_44
action_40 (26) = happyShift action_19
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (16) = happyShift action_43
action_41 (20) = happyShift action_38
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (26) = happyShift action_19
action_42 _ = happyReduce_5

action_43 (15) = happyShift action_9
action_43 (17) = happyShift action_10
action_43 (21) = happyShift action_11
action_43 (24) = happyShift action_12
action_43 (28) = happyShift action_13
action_43 (29) = happyShift action_14
action_43 (8) = happyGoto action_49
action_43 (9) = happyGoto action_7
action_43 (10) = happyGoto action_8
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_15

action_45 (15) = happyShift action_9
action_45 (17) = happyShift action_10
action_45 (21) = happyShift action_11
action_45 (24) = happyShift action_12
action_45 (28) = happyShift action_13
action_45 (29) = happyShift action_14
action_45 (8) = happyGoto action_48
action_45 (9) = happyGoto action_7
action_45 (10) = happyGoto action_8
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (20) = happyShift action_38
action_46 _ = happyReduce_18

action_47 _ = happyReduce_20

action_48 (26) = happyShift action_19
action_48 _ = happyReduce_7

action_49 (26) = happyShift action_19
action_49 _ = happyReduce_6

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 (Eval happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 7 happyReduction_5
happyReduction_5 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Def happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 8 happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Abs happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 8 happyReduction_7
happyReduction_7 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LLet happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (LAs happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App happy_var_1 happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn8
		 (LVar happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (LFirst happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  10 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (LSecond happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 5 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (LTuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn11
		 (Base
	)

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Fun happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn11
		 (Unit
	)

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  12 happyReduction_22
happyReduction_22  =  HappyAbsSyn12
		 ([]
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 30 30 tk (HappyState action) sts stk;
	TEquals -> cont 13;
	TColon -> cont 14;
	TAbs -> cont 15;
	TDot -> cont 16;
	TOpen -> cont 17;
	TClose -> cont 18;
	TComma -> cont 19;
	TArrow -> cont 20;
	TVar happy_dollar_dollar -> cont 21;
	TType -> cont 22;
	TDef -> cont 23;
	TLet -> cont 24;
	TIn -> cont 25;
	TAs -> cont 26;
	TUnit -> cont 27;
	TFirst -> cont 28;
	TSecond -> cont 29;
	_ -> happyError' (tk, [])
	})

happyError_ explist 30 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 :: () => P a -> (a -> P b) -> P b
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> P a
happyError' tk = (\(tokens, explist) -> happyError) tk
parseStmt = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseStmts = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

term = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TType
               | TDef
               | TAbs
               | TDot
               | TOpen
               | TClose 
               | TComma
               | TColon
               | TArrow
               | TEquals
               | TEOF
               | TLet
               | TIn
               | TAs
               | TUnit
               | TFirst
               | TSecond
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                    ('-':('-':cs)) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':('-':cs)) -> consumirBK 0 0 cont cs    
                    ('-':('}':cs)) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':('>':cs)) -> cont TArrow cs
                    ('\\':cs)-> cont TAbs cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('-':('>':cs)) -> cont TArrow cs
                    (')':cs) -> cont TClose cs
                    (',':cs) -> cont TComma cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    unknown -> \line -> Failed $ "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha cs of
                                           ("B",rest)   -> cont TType rest
                                           ("def",rest) -> cont TDef rest
                                           ("let", rest) -> cont TLet rest
                                           ("in", rest) -> cont TIn rest
                                           ("as", rest) -> cont TAs rest
                                           ("unit", rest) -> cont TUnit rest
                                           ("fst", rest) -> cont TFirst rest
                                           ("snd", rest) -> cont TSecond rest
                                           (var,rest)   -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                                                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                                                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs   
                                                              ('-':('}':cs)) -> case anidado of
                                                                                 0 -> \line -> lexer cont cs (line+cl)
                                                                                 _ -> consumirBK (anidado-1) cl cont cs
                                                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                                                              (_:cs) -> consumirBK anidado cl cont cs     
                                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}






# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "/opt/ghc/7.10.3/lib/ghc-7.10.3/include/ghcversion.h" #-}

















{-# LINE 6 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
