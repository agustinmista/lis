module Eval3 (eval) where

import AST

data Result t = Succ t | Fail Error deriving Show

type State = [(Variable, Int)]

data Error = VarNotFound | DivByZero
instance Show Error where
    show DivByZero   = "DivByZero"
    show VarNotFound = "VarNotFound"

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Result Int
lookfor v [] = Fail VarNotFound
lookfor v (x:xs) = if v == fst x
                   then Succ (snd x)
                   else lookfor v xs

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v i [] = [(v,i)]
update v i (x:xs) = if v == fst x
                    then (v,i) : xs
                    else x : (update v i xs)

-- Evalua un programa en el estado nulo
eval :: Comm -> (Result State, Int)
eval p = evalComm p (initState, 0)

-- Evalua un comando en un estado dado
evalComm :: Comm -> (State, Int) -> (Result State, Int)
evalComm comm (s, c) = 
    case comm 
    of Skip             -> (Succ s, c) 
       Let   var  ie    -> case evalIntExp ie s
                           of (Succ val, c1) -> (Succ (update var val s), c1)
                              (Fail err, c1) -> (Fail err, c1)
       Seq   c1   c2    -> case evalComm c1 (s,c)
                           of (Succ res, c1) -> let (res', c') = evalComm c2 (res, c+c1) in (res', c1+c')
                              (Fail err, c1) -> (Fail err, c+c1)
       Cond  cond cT cF -> case evalBoolExp cond s
                           of (Succ res, c1) -> if res then evalComm cT (s, c+c1) else evalComm cF (s, c+c1)
                              (Fail err, c1) -> (Fail err, c+c1)
       While cond cmd   -> case evalBoolExp cond s
                           of (Succ res, c1) -> if res 
                                                then evalComm (Seq cmd (While cond cmd)) (s, c1) 
                                                else (Succ s, c+c1)
                              (Fail err, c1) -> (Fail err, c+c1)
                              
-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> (Result Int, Int)
evalIntExp e s =
    case e 
    of Const  int     -> (Succ int, 0)
       Var    var     -> case lookfor var s
                         of Succ val -> (Succ val, 0)
                            Fail err -> (Fail err, 0)
       UMinus ie      -> case  evalIntExp ie s 
                         of (Succ val, c1) -> (Succ (-val), c1)
                            (Fail err, c1) -> (Fail err, c1)
       Plus   ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 + val2), c1+c2+1)
                            ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2+1)
                            ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2+1)
       Minus  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 - val2), c1+c2+1)
                            ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2+1)
                            ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2+1)
       Times  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 * val2), c1+c2+1)
                            ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2+1)
                            ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2+1)
       Div    ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of ((Succ val1, c1), (Succ val2, c2)) -> if val2 /= 0 
                                                      then (Succ (val1 `div` val2), c1+c2+1) 
                                                      else (Fail DivByZero, c1+c2+1)
                            ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2+1)
                            ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2+1)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> (Result Bool, Int)
evalBoolExp e s =
    case e
    of BFalse      -> (Succ False, 0)
       BTrue       -> (Succ True, 0)
       Eq  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 == val2), c1+c2)
                         ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2)
                         ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2)
       Lt  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 < val2), c1+c2)
                         ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2)
                         ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2)
       Gt  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 > val2), c1+c2)
                         ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2)
                         ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2)
       And be1 be2 -> case (evalBoolExp be1 s, evalBoolExp be2 s)
                      of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 && val2), c1+c2)
                         ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2)
                         ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2)
       Or  be1 be2 -> case (evalBoolExp be1 s, evalBoolExp be2 s)
                      of ((Succ val1, c1), (Succ val2, c2)) -> (Succ (val1 || val2), c1+c2)
                         ((Fail err, c1), (_, c2)) -> (Fail err, c1+c2)
                         ((_, c1), (Fail err, c2)) -> (Fail err, c1+c2)
       Not be      ->  case evalBoolExp be s
                       of (Succ val, c1) -> (Succ (not val), c1)
                          (Fail err, c1) -> (Fail err, c1)