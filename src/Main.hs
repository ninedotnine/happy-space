module Main where
import Control.Monad (forever)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

import ShuntingYard (run_shunting_yard, print_shunting_yard, pretty_show)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then repl
        else if elem "-" args
            then parse_stdin
            else parse_all args

parse_stdin :: IO ()
parse_stdin = do
    input <- getContents
    case run_shunting_yard input of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (pretty_show tree) >> exitSuccess

repl :: IO ()
repl = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    print_shunting_yard input

parse_all :: [String] -> IO ()
parse_all exprs = mapM_ print_shunting_yard exprs
