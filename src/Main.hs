module Main (
    main
) where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Text qualified as Text (pack)
import Data.Text.IO qualified as Text (getContents)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import ShuntingYard

main :: IO ()
main = do
    getArgs >>= \case
        []    -> repl
        ["-"] -> parse_stdin
        args  -> parse_all args

repl :: IO ()
repl = runInputT defaultSettings loop where
    loop = getInputLine "> " >>= \case
        Nothing -> pure ()
        Just input -> liftIO (pep input) >> loop

pep :: String -> IO ()
pep line = unless (all isSpace line) $
    line & Text.pack & parse_eval_print

parse_stdin :: IO ()
parse_stdin = Text.getContents
          <&> run_shunting_yard
          >>= \case
    Left err -> putStrLn (show err)
             >> exitFailure
    Right tree -> putStrLn (eval_show tree)
               >> exitSuccess

parse_all :: [String] -> IO ()
parse_all exprs = mapM_ parse_eval_print (map Text.pack exprs)
