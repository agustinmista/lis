module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable, Int)]


-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
lookfor :: Variable -> State -> Int
lookfor v [] = error "Var not in scope"
lookfor v (x:xs) = if v == fst x
                   then snd x
                   else lookfor v xs

-- Cambia el valor de una variable en un estado
-- (Si la variable no existe la agrega al final) CONSULTAR!
update :: Variable -> Int -> State -> State
update v i [] = [(v,i)]
update v i (x:xs) = if v == fst x
                    then (v,i) : xs
                    else x : (update v i xs)

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm comm s = 
    case comm 
    of Skip             -> s 
       Let   var  ie    -> update var (evalIntExp ie s) s 
       Seq   c1   c2    -> let s' = evalComm c1 s in evalComm c2 s'
       Cond  cond cT cF -> if evalBoolExp cond s then evalComm cT s else evalComm cF s
       While cond c     -> if evalBoolExp cond s then evalComm (Seq c (While cond c)) s else s

-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: IntExp -> State -> Int
evalIntExp e s =
    case e 
    of Const  int     -> int
       Var    var     -> lookfor var s
       UMinus ie      -> -(evalIntExp ie s)
       Plus   ie1 ie2 -> (evalIntExp ie1 s)   +   (evalIntExp ie2 s)
       Minus  ie1 ie2 -> (evalIntExp ie1 s)   -   (evalIntExp ie2 s)
       Times  ie1 ie2 -> (evalIntExp ie1 s)   *   (evalIntExp ie2 s)
       Div    ie1 ie2 -> (evalIntExp ie1 s) `div` (evalIntExp ie2 s)

-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp e s =
    case e
    of BFalse      -> False
       BTrue       -> True
       Eq  ie1 ie2 -> (evalIntExp ie1 s)  == (evalIntExp ie2 s)
       Lt  ie1 ie2 -> (evalIntExp ie1 s)  <  (evalIntExp ie2 s)
       Gt  ie1 ie2 -> (evalIntExp ie1 s)  >  (evalIntExp ie2 s)
       And be1 be2 -> (evalBoolExp be1 s) && (evalBoolExp be2 s)
       Or  be1 be2 -> (evalBoolExp be1 s) || (evalBoolExp be2 s)
       Not be      ->  not (evalBoolExp be s)
