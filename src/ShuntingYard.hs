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

import Control.Monad (when)
import Data.Char (isSpace) -- for trim_spaces
import Data.Fixed qualified as Fixed (mod')
import Data.Functor (void, (<&>))
import Data.Function ((&))
import Data.Ratio (numerator, denominator)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Read (rational)
import GHC.Float (fromRat)
import Text.Parsec qualified as Parsec
import Text.Parsec (Parsec, (<|>), (<?>))

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Precedence = Precedence Integer deriving (Eq, Ord)

data TermToken = Term Rational
               | TightPreOp PrefixOperator
               | SpacedPreOp PrefixOperator
               | LParen

data OperToken = Oper Operator
               | RParen
               | RParenAfterSpace
    deriving Show

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Leaf Rational
         deriving Show

data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             | StackSpacedPreOp PrefixOperator
             | StackTightPreOp PrefixOperator
             deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | FloorDiv
              | Modulo
              | Hihat
              deriving Eq

data PrefixOperator = Negate
                    | Explode
                    deriving Eq

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show

newtype Tree_Stack = Tree_Stack [ASTree] deriving Show

newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

type CuteParser = Parsec Text Stack_State


instance Show Operator where
    show = \case
        Plus     -> "+"
        Minus    -> "-"
        Splat    -> "*"
        Divide   -> "/"
        FloorDiv -> "//"
        Modulo   -> "%"
        Hihat    -> "^"

pref_oper_to_char :: PrefixOperator -> Char
pref_oper_to_char = \case
    Negate  -> '~'
    Explode -> '!'

instance Show PrefixOperator where
    show x = [pref_oper_to_char x]

precedence :: Operator -> Precedence
precedence = \case
    Plus     -> Precedence 6
    Minus    -> Precedence 6
    Splat    -> Precedence 7
    Divide   -> Precedence 7
    FloorDiv -> Precedence 7
    Modulo   -> Precedence 7
    Hihat    -> Precedence 8


-- functions to get the current state
get_op_stack :: CuteParser Oper_Stack
get_op_stack = do
    (stack, _, _) <- Parsec.getState
    pure stack

get_tree_stack :: CuteParser Tree_Stack
get_tree_stack = do
    (_, stack, _) <- Parsec.getState
    pure stack

get_tightness :: CuteParser Tightness
get_tightness = do
    (_, _, tightness) <- Parsec.getState
    pure tightness

-- functions to change or set the current state
oper_stack_push :: StackOp -> CuteParser ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms, b) -> (Oper_Stack (op:ops), terms, b))

oper_stack_set :: [StackOp] -> CuteParser ()
oper_stack_set tokes = Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))

tree_stack_push :: ASTree -> CuteParser ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals, b) -> (ops, Tree_Stack (tree:vals), b))

tree_stack_pop :: CuteParser ASTree
tree_stack_pop = do
    (opers, vals, b) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs, b)
            pure v
        Tree_Stack _ -> Parsec.unexpected "?? did i expect a term?"

set_spacing_tight :: Bool -> CuteParser ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

-- functions that build the ASTree
make_branch :: Operator -> [StackOp] -> CuteParser ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    oper_stack_set tokes

make_twig :: PrefixOperator -> [StackOp] -> CuteParser ()
make_twig op tokes = do
    tree <- tree_stack_pop
    tree_stack_push (Twig op tree)
    oper_stack_set tokes

-- simple spacing-related parsers
respect_spaces :: CuteParser ()
respect_spaces = Parsec.skipMany1 silent_space

ignore_spaces :: CuteParser ()
ignore_spaces = Parsec.skipMany silent_space

silent_space :: CuteParser Char
silent_space = Parsec.char ' ' <?> ""

no_spaces :: String -> CuteParser ()
no_spaces failmsg = (Parsec.try silent_space *> Parsec.unexpected failmsg)
                    <|> pure ()

if_loosely_spaced :: CuteParser () -> CuteParser ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: CuteParser () -> CuteParser ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action

-- term parsers
parse_num :: CuteParser TermToken
parse_num = do
    integer <- Parsec.many1 Parsec.digit <&> Text.pack
    mantissa <- Parsec.option ""
        (Parsec.string "." <> Parsec.many1 Parsec.digit)
        <&> Text.pack
    case (integer <> mantissa) & Read.rational of
        Right (num, "") -> pure $ Term num
        -- these cases should be unreachable:
        -- `integer` can only contain digits;
        -- `mantissa` can only contain a dot then digits (or be empty)
        -- i can think of no reason why this should fail to parse.
        Right _ -> error "unreachable"
        Left _ -> error "unreachable"

parse_prefix_op :: CuteParser TermToken
parse_prefix_op = do
    oper <- parse_oper_symbol
    Parsec.optionMaybe respect_spaces >>= \case
        Nothing -> pure (TightPreOp oper)
        Just () -> pure (SpacedPreOp oper)
    where parse_oper_symbol =
                Parsec.char '!' *> pure Explode
            <|> Parsec.char '~' *> pure Negate
            <?> "prefix operator"

parse_left_paren :: CuteParser TermToken
parse_left_paren = do
    void $ Parsec.lookAhead (Parsec.try parse_l)
    parse_l *> pure LParen
    where parse_l = ignore_spaces *> Parsec.char '('

-- oper parsers
parse_infix_oper :: CuteParser OperToken
parse_infix_oper = do
    Parsec.optionMaybe respect_spaces >>= \case
        Nothing -> do
            if_loosely_spaced (oper_stack_push StackSpace)
            set_spacing_tight True
        Just () -> do
            if_tightly_spaced find_left_space
    oper <- parse_oper_symbol
    if_loosely_spaced (respect_spaces <?> "space after `" ++ show oper ++ "`")
    if_tightly_spaced $ no_spaces ("whitespace after `" ++ show oper ++ "`")
    pure (Oper oper)
    where
        parse_oper_symbol =
                Parsec.char '+' *> pure Plus
            <|> Parsec.char '-' *> pure Minus
            <|> Parsec.char '*' *> pure Splat
            <|> parse_floordiv  *> pure FloorDiv -- must try this before Divide
            <|> Parsec.char '/' *> pure Divide
            <|> Parsec.char '%' *> pure Modulo
            <|> Parsec.char '^' *> pure Hihat
            <?> "infix operator"
        parse_floordiv = Parsec.try (Parsec.string "//")

parse_right_paren :: CuteParser OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    void $ Parsec.char ')'
    pure $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace

-- for finishing up at the end
clean_stack :: CuteParser ()
clean_stack = do
    if_tightly_spaced find_left_space
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> pure ()
        (StackTightPreOp op:tokes) -> do
            make_twig op tokes
            clean_stack
        (StackSpacedPreOp op:tokes) -> do
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
        (result:[]) -> pure result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."

apply_higher_prec_ops :: Precedence -> CuteParser ()
apply_higher_prec_ops current = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> pure ()
        (tok:toks) -> case tok of
            StackSpace -> pure ()
            StackLParen -> pure ()
            StackLParenFollowedBySpace -> pure ()
            StackTightPreOp _ -> do
                undefined -- can this case ever occur?
--                 make_twig op toks
--                 apply_higher_prec_ops current
            StackSpacedPreOp op -> do
                make_twig op toks
                apply_higher_prec_ops current
            StackOp op -> case (precedence op `compare` current) of
                LT -> pure ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current

look_for :: StackOp -> CuteParser ()
look_for thing = do
-- pop stuff off the oper_stack until you find `thing`
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            t | t == thing -> oper_stack_set toks
            StackTightPreOp op -> do
                make_twig op toks
                look_for thing
            StackSpacedPreOp op -> do
                make_twig op toks
                look_for thing
            StackOp op -> do
                make_branch op toks
                look_for thing
            StackLParen ->
                Parsec.parserFail "incorrectly spaced parentheses"
            StackLParenFollowedBySpace ->
                Parsec.parserFail "incorrectly spaced parentheses"
            StackSpace ->
                Parsec.parserFail "incorrect spacing or parentheses"

find_left_space :: CuteParser ()
find_left_space = look_for StackSpace *> set_spacing_tight False

parse_term_token :: CuteParser TermToken
parse_term_token = parse_num <|> parse_left_paren <|> parse_prefix_op

check_for_oper :: CuteParser ()
check_for_oper = void
    $ Parsec.lookAhead
    $ Parsec.try
    $ ignore_spaces *> Parsec.oneOf valid_op_chars
    where valid_op_chars = "+-*/%^"

apply_tight_prefix_opers :: CuteParser ()
apply_tight_prefix_opers = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> pure ()
        (tok:toks) -> case tok of
            StackTightPreOp op -> do
                make_twig op toks
                apply_tight_prefix_opers
            _ -> pure ()

parse_oper_token :: CuteParser OperToken
parse_oper_token =
    (check_for_oper *> apply_tight_prefix_opers *> parse_infix_oper)
    <|> parse_right_paren
    <?> "infix operator"

parse_term :: CuteParser ASTree
parse_term = do
    -- shunting yard, returns a parse tree
    toke <- parse_term_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace
                               *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe respect_spaces
            case spacing of
                Nothing -> oper_stack_push StackLParen
                Just _  -> oper_stack_push StackLParenFollowedBySpace
            parse_term
        Term t -> do
            tree_stack_push (Leaf t)
            parse_oper <|> finish_expr
        TightPreOp op -> do
            oper_stack_push (StackTightPreOp op)
            parse_term
        SpacedPreOp op -> do
            oper_stack_push (StackSpacedPreOp op)
            parse_term

parse_oper :: CuteParser ASTree
parse_oper = do
    parse_oper_token >>= \case
        RParen -> do
            if_tightly_spaced find_left_space
            look_for StackLParen
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> pure ()
            parse_oper <|> finish_expr
        RParenAfterSpace -> do
            if_tightly_spaced find_left_space
            look_for StackLParenFollowedBySpace
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> pure ()
            parse_oper <|> finish_expr
        Oper op -> do
            apply_higher_prec_ops (precedence op)
            oper_stack_push (StackOp op)
            parse_term

-- these are little utilities, unrelated to parsing
pretty_show :: ASTree -> String
pretty_show (Branch oper left right) = concat ["(", show oper, " ", pretty_show left, " ", pretty_show right, ")"]
pretty_show (Twig oper tree) = concat ["(", show oper, " ", pretty_show tree, ")"]
pretty_show (Leaf val) = show val

parse_expression :: CuteParser ASTree
parse_expression = parse_term

run_shunting_yard :: Text -> Either Parsec.ParseError ASTree
run_shunting_yard input = Parsec.runParser
                            parse_expression
                            start_state
                            name
                            (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        name = ""
        trim_spaces = Text.dropWhile isSpace <&> Text.dropWhileEnd isSpace

print_shunting_yard :: Text -> IO ()
print_shunting_yard = run_shunting_yard <&> \case
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (pretty_show tree)

evaluate :: ASTree -> Rational
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
            Divide -> (/)
            FloorDiv -> \x y -> (x/y)
                              & floor
                              & (toRational :: Integer -> Rational)
            Modulo -> Fixed.mod'
            -- uses floating-point exponents, unfortunately.
            Hihat  -> \x y -> fromRat x ** fromRat y
                            & (toRational :: Double -> Rational)


eval_show :: ASTree -> String
eval_show = evaluate <&> show_rational

show_rational :: Rational -> String
show_rational n = if denominator n == 1
    then show (numerator n)
    else show (fromRational n :: Double)

parse_eval_print :: Text -> IO ()
parse_eval_print = run_shunting_yard <&> \case
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (eval_show tree)
