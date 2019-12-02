module Main where
import Control.Monad (forever, unless)
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

import ShuntingYard

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        ["-"] -> parse_stdin
        _     -> parse_all args

repl :: IO ()
repl = forever $ do
    putStr "> "
    hFlush stdout
    input <- Text.getLine
    unless (Text.all isSpace input) (parse_eval_print input)

parse_stdin :: IO ()
parse_stdin = do
    input <- Text.getContents
    case run_shunting_yard input of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (eval_show tree) >> exitSuccess

parse_all :: [String] -> IO ()
parse_all exprs = mapM_ parse_eval_print (map Text.pack exprs)
