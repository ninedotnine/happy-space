import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<|>))
import System.Environment (getArgs)

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Precedence = Precedence Integer deriving (Eq, Ord)

-- newtype Oper_Stack = Oper_Stack {get_oper_stack :: [StackOp]}
newtype Oper_Stack = Oper_Stack [StackOp]
data StackOp = StackLParen | StackOp Operator
-- newtype Tree_Stack = Tree_Stack {get_tree_stack :: [ASTree]}
newtype Tree_Stack = Tree_Stack [ASTree]
type Stack_State = (Oper_Stack, Tree_Stack)

data Token = Term Integer
           | Oper Operator
           | LParen
           | RParen


data ASTree = Branch Operator ASTree ASTree
            | Leaf Integer

data Operator = Plus -- keep this?
              | Minus
              | Splat
              | Divide
              | Modulo
              | Hihat


oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'

{-
char_to_oper :: Char -> Maybe Operator
char_to_oper '+' = Just Plus
char_to_oper '-' = Just Minus
char_to_oper '*' = Just Splat
char_to_oper '/' = Just Divide
char_to_oper '%' = Just Modulo
char_to_oper '^' = Just Hihat
char_to_oper '(' = Just LeftParen
char_to_oper _ = Nothing
-}

get_prec :: Operator -> Precedence
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8


-- stack functions
oper_stack_push :: StackOp -> Parsec String Stack_State ()
-- oper_stack_push :: Token -> Parsec String Stack_State ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms) -> (Oper_Stack (op:ops), terms))

{-
-- oper_stack_pop :: Parsec String Stack_State Operator
oper_stack_pop = do
    (opers, terms) <- Parsec.getState
    case opers of
        Oper_Stack (op:ops) -> do
            Parsec.setState (Oper_Stack ops, terms)
            return op
        Oper_Stack (_) -> Parsec.unexpected "cock"
--         (_) -> Parsec.parserFail "cock"
-}


{-
oper_stack_peek :: Parsec String Stack_State (Maybe StackOp)
oper_stack_peek = do
    opers <- fst <$> Parsec.getState
    case opers of
        Oper_Stack (op:_) -> return (Just op)
        Oper_Stack (_)   -> return Nothing
-}


{-
oper_stack_non_empty :: Parsec String Stack_State (Bool)
oper_stack_non_empty = do
    (Oper_Stack opers) <- fst <$> Parsec.getState
    return . not $ null opers
-}

tree_stack_push :: ASTree -> Parsec String Stack_State ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals) -> (ops, Tree_Stack (tree:vals)))

tree_stack_pop :: Parsec String Stack_State ASTree
tree_stack_pop = do
    (opers, vals) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs)
            return v
        Tree_Stack _ -> Parsec.unexpected "cockterm"
--         (_) -> Parsec.parserFail "cock"

{-
tree_stack_peek :: Parsec String Stack_State (Maybe ASTree)
tree_stack_peek = do
    trees <- snd <$> Parsec.getState
    case trees of
        Tree_Stack (top:_) -> return (Just top)
        Tree_Stack (_)     -> return Nothing
-}


{-
stack_oper_comes_before :: Token -> Parsec String Stack_State Bool
stack_oper_comes_before (Oper op) = do
    top_of_stack <- oper_stack_peek
    case top_of_stack of
        Nothing                   -> return False
        (Just StackLParen)        -> return False
        (Just (StackOp stack_op)) -> return (get_prec stack_op > get_prec op)
stack_oper_comes_before (_) = error "oop"
-}



parse_num :: Parsec String Stack_State Integer
parse_num = read <$> Parsec.many1 Parsec.digit

parse_oper :: Parsec String Stack_State Operator
parse_oper = do
    (Parsec.char '+' *> return Plus)
    <|> (Parsec.char '-' *> return Minus)
    <|> (Parsec.char '*' *> return Splat)


parse_token :: Parsec String Stack_State Token
parse_token =
    (Parsec.char '(' *> return LParen)
    <|> (Parsec.char ')' *> return RParen)
    <|> Term <$> parse_num
    <|> Oper <$> parse_oper


make_branch :: Operator -> [StackOp] -> Parsec String Stack_State ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    Parsec.modifyState (\(_,s2) -> (Oper_Stack tokes, s2))


clean_stack :: Parsec String Stack_State ()
clean_stack = do
    Oper_Stack op_stack <- fst <$> Parsec.getState
    case op_stack of
        [] -> return ()
        (tok:tokes) -> case tok of
            StackLParen -> error "fix types. did i fix the types though"
            StackOp op -> do
                make_branch op tokes
                clean_stack


finish_expr :: Parsec String Stack_State ASTree
finish_expr = do
    _ <- Parsec.optional (Parsec.char '\n')
    clean_stack
    Tree_Stack tree <- snd <$> Parsec.getState
    case tree of
        [] -> error "what"
        (result:[]) -> return result
        _ -> do
            error "extras? what"

apply_higher_prec_ops :: Precedence -> Parsec String Stack_State ()
apply_higher_prec_ops current = do
    op_stack <- fst <$> Parsec.getState
    case op_stack of
        Oper_Stack [] -> return ()
        Oper_Stack (tok:toks) -> case tok of
            StackLParen -> return ()
            StackOp op -> case (get_prec op `compare` current) of
                LT -> return ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current



find_l_paren :: Parsec String Stack_State ()
find_l_paren = do
    Oper_Stack op_stack <- fst <$> Parsec.getState
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            StackLParen -> Parsec.modifyState (\(_,s2) -> (Oper_Stack toks,s2)) *> return ()
            StackOp op -> do
                make_branch op toks
                find_l_paren



parse_expression :: Parsec String Stack_State ASTree
parse_expression = do
    -- shunting yard, returns a parse tree
    toke <- parse_token
    case toke of
        LParen -> oper_stack_push StackLParen *> parse_expression
        RParen -> find_l_paren *> (parse_expression <|> finish_expr)
        Term t -> tree_stack_push (Leaf t) *> (parse_expression <|> finish_expr)
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            parse_expression


run_shunting_yard :: String -> String
run_shunting_yard input = case Parsec.runParser parse_expression (Oper_Stack [],Tree_Stack []) "input" input of
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
