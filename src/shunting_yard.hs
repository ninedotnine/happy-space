-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.


import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<|>))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Control.Monad (when)

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Precedence = Precedence Integer deriving (Eq, Ord)

data Token = Term Integer
           | Oper Operator
           | LParen
           | RParen
           | RParenAfterSpace
    deriving Show

data ASTree = Branch Operator ASTree ASTree
            | Leaf Integer
         deriving Show

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show
data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             deriving Show

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | Modulo
              | Hihat

newtype Tree_Stack = Tree_Stack [ASTree] deriving Show

newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

valid_op_chars :: String
valid_op_chars = "+-*/%^"

oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'

instance Show Operator where
    show x = [oper_to_char x]

get_prec :: Operator -> Precedence
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8


-- functions to get the current state
get_op_stack :: Parsec String Stack_State Oper_Stack
get_op_stack = do
    (stack, _, _) <- Parsec.getState
    return stack

get_tree_stack :: Parsec String Stack_State Tree_Stack
get_tree_stack = do
    (_, stack, _) <- Parsec.getState
    return stack

get_tightness :: Parsec String Stack_State Tightness
get_tightness = do
    (_, _, tightness) <- Parsec.getState
    return tightness

-- stack functions
oper_stack_push :: StackOp -> Parsec String Stack_State ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms, b) -> (Oper_Stack (op:ops), terms, b))


tree_stack_push :: ASTree -> Parsec String Stack_State ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals, b) -> (ops, Tree_Stack (tree:vals), b))

tree_stack_pop :: Parsec String Stack_State ASTree
tree_stack_pop = do
    (opers, vals, b) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs, b)
            return v
        Tree_Stack _ -> Parsec.unexpected "?? did i expect a term?"


begin_spaced_prec :: Parsec String Stack_State ()
begin_spaced_prec = do
    if_loosely_spaced (oper_stack_push StackSpace)
    set_spacing_tight True


set_spacing_tight :: Bool -> Parsec String Stack_State ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

read_spaces :: Parsec String Stack_State [Char]
read_spaces = Parsec.many1 (Parsec.char ' ')

ignore_spaces :: Parsec String Stack_State ()
ignore_spaces = Parsec.many (Parsec.char ' ') *> return ()

parse_num :: Parsec String Stack_State Token
parse_num = Term <$> read <$> Parsec.many1 Parsec.digit

parse_oper :: Parsec String Stack_State Token
parse_oper = do
    spacing <- Parsec.optionMaybe read_spaces
    case spacing of
        Nothing -> begin_spaced_prec
        Just _  -> do
            if_tightly_spaced find_left_space
    oper <- parse_oper_symbol
    if_loosely_spaced (read_spaces *> return ()) <|> Parsec.parserFail ("invalid whitespace around `" ++ show oper ++ "`")
    if_tightly_spaced $ no_spaces ("whitespace after `" ++ show oper ++ "`")
    return (Oper oper)

parse_oper_symbol :: Parsec String Stack_State Operator
parse_oper_symbol =
    Parsec.char '+' *> return Plus   <|>
    Parsec.char '-' *> return Minus  <|>
    Parsec.char '*' *> return Splat  <|>
    Parsec.char '/' *> return Divide <|>
    Parsec.char '%' *> return Modulo <|>
    Parsec.char '^' *> return Hihat

no_spaces :: String -> Parsec String Stack_State ()
no_spaces failmsg = Parsec.try ((Parsec.try (Parsec.char ' ') *> Parsec.unexpected failmsg) <|> return ())

parse_left_paren :: Parsec String Stack_State Token
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen

parse_right_paren :: Parsec String Stack_State Token
parse_right_paren = do
    spacing <- Parsec.optionMaybe read_spaces
    _ <- Parsec.char ')'
    return $ case spacing of
        Nothing -> RParen
        Just _  -> RParenAfterSpace

check_for_oper :: Parsec String Stack_State ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars)) *> return ()

parse_token :: Parsec String Stack_State Token
parse_token = do
    parse_num <|> (check_for_oper *> parse_oper) <|> parse_left_paren <|> parse_right_paren


make_branch :: Operator -> [StackOp] -> Parsec String Stack_State ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))


clean_stack :: Parsec String Stack_State ()
clean_stack = do
    if_tightly_spaced find_left_space
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (StackOp op:tokes) -> do
            make_branch op tokes
            clean_stack
        _ -> Parsec.parserFail "incorrect whitespace or parens?"


finish_expr :: Parsec String Stack_State ASTree
finish_expr = do
    ignore_spaces
    Parsec.optional Parsec.newline
    Parsec.eof
    clean_stack
    Tree_Stack tree <- get_tree_stack
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> return result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."

apply_higher_prec_ops :: Precedence -> Parsec String Stack_State ()
apply_higher_prec_ops current = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackSpace -> return ()
            StackLParen -> return ()
            StackLParenFollowedBySpace -> return ()
            StackOp op -> case (get_prec op `compare` current) of
                LT -> return ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current


find_left_paren :: Parsec String Stack_State ()
find_left_paren = do
-- pop stuff off the oper_stack until you find a StackLParen
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.modifyState (\(_,s2,b) -> (Oper_Stack toks,s2,b)) *> return ()
            StackLParenFollowedBySpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackOp op -> do
                make_branch op toks
                find_left_paren

find_left_paren_spaced :: Parsec String Stack_State ()
find_left_paren_spaced = do
-- pop stuff off the oper_stack until you find a StackLParen
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.parserFail "incorrectly spaced parentheses"
            StackLParenFollowedBySpace -> Parsec.modifyState (\(_,s2,b) -> (Oper_Stack toks,s2,b))
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackOp op -> do
                make_branch op toks
                find_left_paren_spaced


find_left_space :: Parsec String Stack_State ()
find_left_space = do
-- pop stuff off the oper_stack until you find a StackSpace
-- and finally set Tight to False
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "incorrect spacing"
        (tok:toks) -> case tok of
            StackSpace -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack toks,s2,Tight False))
            StackLParen -> Parsec.parserFail "FIXME this should be allowed"
            StackLParenFollowedBySpace -> Parsec.parserFail "i feel like these should not be allowed actually"
            StackOp op -> do
                make_branch op toks
                find_left_space

if_loosely_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action


parse_expression :: Parsec String Stack_State ASTree
parse_expression = do
    -- shunting yard, returns a parse tree
    toke <- parse_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe read_spaces
            case spacing of
                Nothing -> oper_stack_push StackLParen
                Just _  -> oper_stack_push StackLParenFollowedBySpace
            parse_expression
        RParen -> do
            if_tightly_spaced find_left_space
            find_left_paren
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack ops, s2, Tight True))
                _ -> return ()
            parse_expression <|> finish_expr
        RParenAfterSpace -> do
            if_tightly_spaced find_left_space
            find_left_paren_spaced
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack ops, s2, Tight True))
                _ -> return ()
            parse_expression <|> finish_expr
        Term t -> tree_stack_push (Leaf t) *> (parse_expression <|> finish_expr)
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            parse_expression


run_shunting_yard :: String -> IO ()
run_shunting_yard input = case Parsec.runParser (ignore_spaces *> parse_expression) start_state "input" input of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (pretty_show tree) >> exitSuccess
    where start_state = (Oper_Stack [],Tree_Stack [],Tight False)


pretty_show :: ASTree -> String
pretty_show (Branch oper left right) = "(" ++ show oper ++ " "  ++ pretty_show left ++ " " ++ pretty_show right ++ ")"
pretty_show (Leaf val) = show val


main :: IO ()
-- main = interact run_shunting_yard >> putChar '\n'
main = do
    args <- getArgs
    if length args == 1
        then
            mapM_ run_shunting_yard args
        else do
            input <- getContents
            run_shunting_yard input
