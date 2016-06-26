{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT
Maintainer: manuelalejandropm@gmail.com
Main module of the bathroom excercise
-}

import System.Environment (getArgs)

import Queue (runClassic)

---- MAIN PROGRAM

main :: IO ()
main = do
  args <- getArgs
  if null args
    then usage
    else case head args of
      "classic" -> runClassic
      "transactional" -> error "Not Implemented"
      _ -> usage

usage :: IO ()
usage = do
  putStrLn $
    "El programa acepta un argumento por linea de comandos\n"
    ++ "para indicar el tipo de ejecucion:\n"
    ++ "  ./Bathroom classic\n"
    ++ "  ./Bathroom transactional"
