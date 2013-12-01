module Main where

import Parser
import Interpreter
import Text.Parsec.String
import Text.Parsec.Prim
import System.Exit
import System.Environment
import Control.Monad(when)

parseFile :: FilePath -> Parser a -> IO a
parseFile file p =
  do programFile  <- readFile file
     case parse p "" programFile of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

main :: IO ()
main = do
    args <- getArgs
    when (null args || elem "-h" args || length args /= 1) $ 
         putStrLn "Usage: blaise [-h] [file ..]" >> exitSuccess

    parsedProgram <- parseFile (head args) program
    interpretate parsedProgram
