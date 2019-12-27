-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

module ShuntingYard (
    pretty_show,
    run_shunting_yard,
    print_shunting_yard,
    evaluate,
    eval_show,
    parse_eval_print
) where

import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<|>), (<?>))

-- for trim_spaces
import Data.Char (isSpace)
import Data.Functor ((<&>))

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Monad (when)

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Precedence = Precedence Integer deriving (Eq, Ord)

data TermToken = Term Integer
               | PreOp PrefixOperator
               | LParen

data OperToken = Oper Operator
               | RParen
               | RParenAfterSpace
    deriving Show

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Leaf Integer
         deriving Show

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show
data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             | StackPreOp PrefixOperator
             deriving Show

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | Modulo
              | Hihat

data PrefixOperator = Negate
                    | Explode

newtype Tree_Stack = Tree_Stack [ASTree] deriving Show

newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

type CuteParser = Parsec Text Stack_State

oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'

instance Show Operator where
    show x = [oper_to_char x]

pref_oper_to_char :: PrefixOperator -> Char
pref_oper_to_char Negate = '~'
pref_oper_to_char Explode = '!'

instance Show PrefixOperator where
    show x = [pref_oper_to_char x]

get_prec :: Operator -> Precedence
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8


-- functions to get the current state
get_op_stack :: CuteParser Oper_Stack
get_op_stack = do
    (stack, _, _) <- Parsec.getState
    return stack

get_tree_stack :: CuteParser Tree_Stack
get_tree_stack = do
    (_, stack, _) <- Parsec.getState
    return stack

get_tightness :: CuteParser Tightness
get_tightness = do
    (_, _, tightness) <- Parsec.getState
    return tightness

-- stack functions
oper_stack_push :: StackOp -> CuteParser ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms, b) -> (Oper_Stack (op:ops), terms, b))


tree_stack_push :: ASTree -> CuteParser ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals, b) -> (ops, Tree_Stack (tree:vals), b))

tree_stack_pop :: CuteParser ASTree
tree_stack_pop = do
    (opers, vals, b) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs, b)
            return v
        Tree_Stack _ -> Parsec.unexpected "?? did i expect a term?"


begin_spaced_prec :: CuteParser ()
begin_spaced_prec = do
    if_loosely_spaced (oper_stack_push StackSpace)
    set_spacing_tight True


set_spacing_tight :: Bool -> CuteParser ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

respect_spaces :: CuteParser ()
respect_spaces = Parsec.skipMany1 silent_space

ignore_spaces :: CuteParser ()
ignore_spaces = Parsec.skipMany silent_space

silent_space :: CuteParser Char
silent_space = Parsec.char ' ' <?> ""

parse_num :: CuteParser TermToken
parse_num = Term <$> read <$> Parsec.many1 Parsec.digit

parse_prefix_op :: CuteParser TermToken
parse_prefix_op = PreOp <$> (
    Parsec.char '!' *> return Explode <|>
    Parsec.char '~' *> return Negate
    ) <?> "prefix operator"

parse_oper :: CuteParser OperToken
parse_oper = do
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> begin_spaced_prec
        Just () -> do
            if_tightly_spaced find_left_space
    oper <- parse_oper_symbol
    if_loosely_spaced (respect_spaces <?> "space after `" ++ show oper ++ "`")
    if_tightly_spaced $ no_spaces ("whitespace after `" ++ show oper ++ "`")
    return (Oper oper)

parse_oper_symbol :: CuteParser Operator
parse_oper_symbol =
    Parsec.char '+' *> return Plus   <|>
    Parsec.char '-' *> return Minus  <|>
    Parsec.char '*' *> return Splat  <|>
    Parsec.char '/' *> return Divide <|>
    Parsec.char '%' *> return Modulo <|>
    Parsec.char '^' *> return Hihat <?> "infix operator"

no_spaces :: String -> CuteParser ()
no_spaces failmsg = Parsec.try ((Parsec.try silent_space *> Parsec.unexpected failmsg) <|> return ())

parse_left_paren :: CuteParser TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen

parse_right_paren :: CuteParser OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    _ <- Parsec.char ')'
    return $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace


make_branch :: Operator -> [StackOp] -> CuteParser ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))

make_twig :: PrefixOperator -> [StackOp] -> CuteParser ()
make_twig op tokes = do
    tree <- tree_stack_pop
    tree_stack_push (Twig op tree)
    Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))

clean_stack :: CuteParser ()
clean_stack = do
    if_tightly_spaced find_left_space
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (StackPreOp op:tokes) -> do
            make_twig op tokes
            clean_stack
        (StackOp op:tokes) -> do
            make_branch op tokes
            clean_stack
        _ -> Parsec.parserFail "incorrect whitespace or parens?"


finish_expr :: CuteParser ASTree
finish_expr = do
    ignore_spaces
    Parsec.optional Parsec.newline <?> ""
    Parsec.eof <?> ""
    clean_stack
    Tree_Stack tree <- get_tree_stack
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> return result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."

apply_higher_prec_ops :: Precedence -> CuteParser ()
apply_higher_prec_ops current = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackSpace -> return ()
            StackLParen -> return ()
            StackLParenFollowedBySpace -> return ()
            (StackPreOp op) -> make_twig op toks
            StackOp op -> case (get_prec op `compare` current) of
                LT -> return ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current


find_left_paren :: CuteParser ()
find_left_paren = do
-- pop stuff off the oper_stack until you find a StackLParen
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.modifyState (\(_,s2,b) -> (Oper_Stack toks,s2,b)) *> return ()
            StackLParenFollowedBySpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            (StackPreOp op) -> do
                make_twig op toks
                find_left_paren
            StackOp op -> do
                make_branch op toks
                find_left_paren

find_left_paren_spaced :: CuteParser ()
find_left_paren_spaced = do
-- pop stuff off the oper_stack until you find a StackLParenFollowedBySpace
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.parserFail "incorrectly spaced parentheses"
            StackLParenFollowedBySpace -> Parsec.modifyState (\(_,s2,b) -> (Oper_Stack toks,s2,b))
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            (StackPreOp op) -> do
                make_twig op toks
                find_left_paren_spaced
            StackOp op -> do
                make_branch op toks
                find_left_paren_spaced


find_left_space :: CuteParser ()
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
            (StackPreOp op) -> do
                make_twig op toks
                find_left_space
            StackOp op -> do
                make_branch op toks
                find_left_space

if_loosely_spaced :: CuteParser () -> CuteParser ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: CuteParser () -> CuteParser ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action

parse_term_token :: CuteParser TermToken
parse_term_token = parse_num <|> parse_left_paren <|> parse_prefix_op

check_for_oper :: CuteParser ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars)) *> return ()
    where valid_op_chars = "+-*/%^"

parse_oper_token :: CuteParser OperToken
parse_oper_token = (check_for_oper *> parse_oper) <|> parse_right_paren <?> "infix operator"

parse_expression :: CuteParser ASTree
parse_expression = expect_term

expect_term :: CuteParser ASTree
expect_term = do
    -- shunting yard, returns a parse tree
    toke <- parse_term_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe respect_spaces
            case spacing of
                Nothing -> oper_stack_push StackLParen
                Just _  -> oper_stack_push StackLParenFollowedBySpace
            expect_term
        Term t -> do
            tree_stack_push (Leaf t)
            expect_infix_op <|> finish_expr
        PreOp op -> do
            oper_stack_push (StackPreOp op)
--             tree_stack_push (Twig op)
            expect_term

expect_infix_op :: CuteParser ASTree
expect_infix_op = do
    toke <- parse_oper_token
    case toke of
        RParen -> do
            if_tightly_spaced find_left_space
            find_left_paren
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack ops, s2, Tight True))
                _ -> return ()
            expect_infix_op <|> finish_expr
        RParenAfterSpace -> do
            if_tightly_spaced find_left_space
            find_left_paren_spaced
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack ops, s2, Tight True))
                _ -> return ()
            expect_infix_op <|> finish_expr
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            expect_term


pretty_show :: ASTree -> String
pretty_show (Branch oper left right) = concat ["(", show oper, " ", pretty_show left, " ", pretty_show right, ")"]
pretty_show (Twig oper tree) = concat ["(", show oper, " ", pretty_show tree, ")"]
pretty_show (Leaf val) = show val

run_shunting_yard :: Text -> Either Parsec.ParseError ASTree
run_shunting_yard input = Parsec.runParser parse_expression start_state "input" (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = Text.dropWhile isSpace <&> Text.dropWhileEnd isSpace

print_shunting_yard :: Text -> IO ()
print_shunting_yard input = case run_shunting_yard input of
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (pretty_show tree)

evaluate :: ASTree -> Integer
evaluate (Leaf x) = x
evaluate (Twig op tree) = operate (evaluate tree)
    where operate = case op of
            Negate -> negate
            Explode -> (\n -> product [1..n])
evaluate (Branch op left right) = evaluate left `operate` evaluate right
    where operate = case op of
            Plus   -> (+)
            Minus  -> (-)
            Splat  -> (*)
            Divide -> div
            Modulo -> mod
            Hihat  -> (^)

eval_show :: ASTree -> String
eval_show = evaluate <&> show

parse_eval_print :: Text -> IO ()
parse_eval_print input = case run_shunting_yard input of
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (eval_show tree)
