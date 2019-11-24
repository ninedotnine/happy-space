import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<|>))
import System.Environment (getArgs)

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
    deriving Show

data ASTree = Branch Operator ASTree ASTree
            | Leaf Integer
         deriving Show

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show
data StackOp = StackLParen
             | StackLSpace
             | StackRSpace
             | StackOp Operator
             deriving Show

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | Modulo
              | Hihat
              deriving Show

newtype Tree_Stack = Tree_Stack [ASTree] deriving Show

newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)


oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'

get_prec :: Operator -> Precedence
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8



fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

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
    if_loosely_spaced (oper_stack_push StackLSpace)
    set_spacing_tight True


set_spacing_tight :: Bool -> Parsec String Stack_State ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

read_spaces :: Parsec String Stack_State [Char]
read_spaces = Parsec.many1 (Parsec.char ' ')

ignore_spaces :: Parsec String Stack_State ()
ignore_spaces = Parsec.many (Parsec.char ' ') *> return ()

parse_num :: Parsec String Stack_State Integer
parse_num = read <$> Parsec.many1 Parsec.digit

parse_oper :: Parsec String Stack_State Operator
parse_oper = do
    spacing <- Parsec.optionMaybe read_spaces
    case spacing of
        Nothing -> begin_spaced_prec
        Just _  -> do
            if_tightly_spaced find_left_space
    oper <- (Parsec.char '+' *> return Plus) <|> (Parsec.char '-' *> return Minus) <|> (Parsec.char '*' *> return Splat)
    if_loosely_spaced (read_spaces *> return ())
    return oper

parse_left_paren :: Parsec String Stack_State Token
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen

parse_right_paren :: Parsec String Stack_State Token
parse_right_paren = do
    ignore_spaces *> Parsec.char ')' *> return RParen

check_for_oper :: Parsec String Stack_State ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf "+-*")) *> return ()

parse_token :: Parsec String Stack_State Token
parse_token = do
    (Term <$> parse_num) <|> (check_for_oper *> (Oper <$> parse_oper)) <|> parse_left_paren <|> parse_right_paren


make_branch :: Operator -> [StackOp] -> Parsec String Stack_State ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))


clean_stack :: Parsec String Stack_State ()
clean_stack = do
    if_tightly_spaced find_left_space
    Oper_Stack op_stack <- fst3 <$> Parsec.getState
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
    clean_stack
    Tree_Stack tree <- snd3 <$> Parsec.getState
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> return result
        _ -> do
            error "extras? what"

apply_higher_prec_ops :: Precedence -> Parsec String Stack_State ()
apply_higher_prec_ops current = do
    Oper_Stack op_stack <- fst3 <$> Parsec.getState
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackLSpace -> return ()
            StackRSpace -> error "huh what"
            StackLParen -> return ()
            StackOp op -> case (get_prec op `compare` current) of
                LT -> return ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current


find_left_paren :: Parsec String Stack_State ()
find_left_paren = do
-- pop stuff off the oper_stack until you find a StackLParen
    Oper_Stack op_stack <- fst3 <$> Parsec.getState
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.modifyState (\(_,s2,b) -> (Oper_Stack toks,s2,b)) *> return ()
            StackRSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackLSpace -> Parsec.parserFail "incorrect spacing or parentheses"
            StackOp op -> do
                make_branch op toks
                find_left_paren

find_left_space :: Parsec String Stack_State ()
find_left_space = do
-- pop stuff off the oper_stack until you find a StackLSpace
-- and finally set Tight to False
    Oper_Stack op_stack <- fst3 <$> Parsec.getState
    case op_stack of
        [] -> Parsec.unexpected "incorrect spacing"
        (tok:toks) -> case tok of
            StackLSpace -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack toks,s2,Tight False))
            StackRSpace -> error "uecoa"
            StackLParen -> Parsec.parserFail "FIXME this should be allowed"
            StackOp op -> do
                make_branch op toks
                find_left_space

if_loosely_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_loosely_spaced action = do
    Tight spaced <- trd3 <$> Parsec.getState
    (when (not spaced)) action

if_tightly_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_tightly_spaced action = do
    Tight spaced <- trd3 <$> Parsec.getState
    when spaced action


parse_expression :: Parsec String Stack_State ASTree
parse_expression = do
    -- shunting yard, returns a parse tree
    toke <- parse_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackRSpace *> set_spacing_tight False)
            oper_stack_push StackLParen
            ignore_spaces
            parse_expression
        RParen -> do
            if_tightly_spaced find_left_space
            find_left_paren
            Oper_Stack stack_ops <- fst3 <$> Parsec.getState
            case stack_ops of
                (StackRSpace:ops) -> Parsec.modifyState (\(_,s2,_) -> (Oper_Stack ops, s2, Tight True))
                _ -> return ()
            parse_expression <|> finish_expr
        Term t -> tree_stack_push (Leaf t) *> (parse_expression <|> finish_expr)
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            parse_expression


run_shunting_yard :: String -> String
run_shunting_yard input = case Parsec.runParser (ignore_spaces *> parse_expression) (Oper_Stack [],Tree_Stack [],Tight False) "input" input of
    Left err -> show err
    Right tree -> pretty_print tree


pretty_print :: ASTree -> String
pretty_print (Branch oper left right) = "(" ++ [oper_to_char oper] ++ " "  ++
    pretty_print left ++ " " ++ pretty_print right ++ ")"
pretty_print (Leaf val) = show val


main :: IO ()
-- main = interact run_shunting_yard >> putChar '\n'
main = do
    getArgs >>= \args -> if length args < 1
    then
        interact run_shunting_yard >> putChar '\n'
    else mapM_ putStrLn $ run_shunting_yard <$> args
