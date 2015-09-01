module Eval2 (eval) where

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
eval :: Comm -> Result State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
evalComm :: Comm -> State -> Result State
evalComm comm s = 
    case comm 
    of Skip             -> Succ s 
       Let   var  ie    -> case evalIntExp ie s 
                           of Succ val -> Succ (update var val s)
                              Fail err -> Fail err
       Seq   c1   c2    -> case evalComm c1 s
                           of Succ res -> evalComm c2 res
                              Fail err -> Fail err
       Cond  cond cT cF -> case evalBoolExp cond s
                           of Succ res -> if res then evalComm cT s else evalComm cF s
                              Fail err -> Fail err
       While cond c     -> case evalBoolExp cond s
                           of Succ res -> if res then evalComm (Seq c (While cond c)) s else Succ s
                              Fail err -> Fail err
                              
-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Result Int
evalIntExp e s =
    case e 
    of Const  int     -> Succ int
       Var    var     -> case lookfor var s
                         of Succ val -> Succ val
                            Fail err -> Fail err
       UMinus ie      -> case  evalIntExp ie s 
                         of Succ val -> Succ (-val)
                            Fail err -> Fail err
       Plus   ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of (Succ val1, Succ val2) -> Succ (val1 + val2)
                            (Fail err, _) -> Fail err
                            (_, Fail err) -> Fail err
       Minus  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of (Succ val1, Succ val2) -> Succ (val1 - val2)
                            (Fail err, _) -> Fail err
                            (_, Fail err) -> Fail err
       Times  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of (Succ val1, Succ val2) -> Succ (val1 * val2)
                            (Fail err, _) -> Fail err
                            (_, Fail err) -> Fail err
       Div    ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                         of (Succ val1, Succ val2) -> if val2 /= 0 then Succ (val1 `div` val2) else Fail DivByZero
                            (Fail err, _) -> Fail err
                            (_, Fail err) -> Fail err

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Result Bool
evalBoolExp e s =
    case e
    of BFalse      -> Succ False
       BTrue       -> Succ True
       Eq  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of (Succ val1, Succ val2) -> Succ (val1 == val2)
                         (Fail err, _) -> Fail err
                         (_, Fail err) -> Fail err
       Lt  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of (Succ val1, Succ val2) -> Succ (val1 < val2)
                         (Fail err, _) -> Fail err
                         (_, Fail err) -> Fail err
       Gt  ie1 ie2 -> case (evalIntExp ie1 s, evalIntExp ie2 s)
                      of (Succ val1, Succ val2) -> Succ (val1 > val2)
                         (Fail err, _) -> Fail err
                         (_, Fail err) -> Fail err
       And be1 be2 -> case (evalBoolExp be1 s, evalBoolExp be2 s)
                      of (Succ val1, Succ val2) -> Succ (val1 && val2)
                         (Fail err, _) -> Fail err
                         (_, Fail err) -> Fail err
       Or  be1 be2 -> case (evalBoolExp be1 s, evalBoolExp be2 s)
                      of (Succ val1, Succ val2) -> Succ (val1 || val2)
                         (Fail err, _) -> Fail err
                         (_, Fail err) -> Fail err
       Not be      ->  case evalBoolExp be s
                       of Succ val -> Succ (not val)
                          Fail err -> Fail err