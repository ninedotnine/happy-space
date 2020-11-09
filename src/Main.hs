module Main where
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)
import qualified Data.Text as Text (pack)
import qualified Data.Text.IO as Text (getContents)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import ShuntingYard

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        ["-"] -> parse_stdin
        _     -> parse_all args

repl :: IO ()
repl = runInputT defaultSettings loop where
    loop = do
        m_input <- getInputLine "> "
        case m_input of
            Nothing -> pure ()
            Just input -> liftIO (pep input) >> loop

pep :: String -> IO ()
pep line = unless (all isSpace line) (parse_eval_print (Text.pack line))
--
parse_stdin :: IO ()
parse_stdin = do
    input <- Text.getContents
    case run_shunting_yard input of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (eval_show tree) >> exitSuccess

parse_all :: [String] -> IO ()
parse_all exprs = mapM_ parse_eval_print (map Text.pack exprs)
