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
evalComm c state = case c 
                   of Skip                  -> error "Skip!"
                      Let var iexp          -> error "Let!"
                      Seq comm1 comm2       -> error "Seq!"
                      Cond cond commT commF -> error "Cond!"
                      While cond comm       -> error "While!"

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Int
evalIntExp = undefined

-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp = undefined
