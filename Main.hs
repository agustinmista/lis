module Main where

import System.Environment (getArgs)
import Parser(parser)
-- El modulo Parser tambien exporta una funcion 
-- 
--   parser :: String -> Comm
--
-- que convierte una cadena de caracteres que representa un programa LIS en una 
-- expresion de tipo Comm.


-- Modificar este import para usar diferentes evaluadores
import Eval3
---------------------------------------------------------

main :: IO ()
main = do arg:_ <- getArgs
          run arg

-- Ejecuta un programa a partir de su archivo fuente
run :: [Char] -> IO ()
run ifile = 
    do
    s <- readFile ifile
    (print . eval) (parser s)
