// parses an expression into an abstract syntax tree using the shunting yard algorithm
// can't handle spaces or parens and accepts single-digit numbers only, lol

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static const char * input_pointer;

static bool theres_more_input(void) {
    return (*input_pointer != '\0') && (*input_pointer != '\n');
}

static void __attribute__((noreturn)) error(char * str) {
    printf("error: %s\n", str);
    exit(EXIT_FAILURE);
}

struct stack_obj {
    struct ASTNode * obj;
    struct stack_obj * next;
};

enum token_assoc {
    RIGHT_ASSOC,
    LEFT_ASSOC
};

enum token_type {
    TOKEN_TYPE_OPERATOR,
    TOKEN_TYPE_TERM,
    TOKEN_TYPE_L_PAREN,
    TOKEN_TYPE_R_PAREN,
};

struct ASTNode {
    int precedence;                 // only if TOKEN_TYPE_OPERATOR
    enum token_assoc associativity; // only if TOKEN_TYPE_OPERATOR
    struct ASTNode * children_right; // only if TOKEN_TYPE_OPERATOR
    struct ASTNode * children_left; // only if TOKEN_TYPE_OPERATOR
    enum token_type type;
    char val;
};


static void stack_push(struct stack_obj ** stack_p, struct ASTNode * obj) {
    if (stack_p == NULL) {
        error("hosed pointer");
    }
    struct stack_obj * new_node = malloc(sizeof(struct stack_obj));
    assert(new_node);
    new_node->obj = obj;
    new_node->next = *stack_p;
    *stack_p = new_node;
}

static struct ASTNode * stack_pop(struct stack_obj ** stack_p) {
    if (stack_p == NULL) {
        error("hosed pointer");
    } else if (*stack_p == NULL) {
        error("empty stack");
    }
    struct stack_obj * ptr = *stack_p;
    struct ASTNode * node = (*stack_p)->obj;
    *stack_p = (*stack_p)->next;
    free(ptr);
    return node;
}

static struct ASTNode * stack_peek(const struct stack_obj * stack) {
    if (stack == NULL) {
        error("empty stack");
    }
    return stack->obj;
}

static void pretty_print(const struct ASTNode * tree) {
    if (tree->type == TOKEN_TYPE_OPERATOR) {
        putchar('(');
        putchar(tree->val);
        putchar(' ');
        pretty_print(tree->children_left);
        putchar(' ');
        pretty_print(tree->children_right);
        putchar(')');
    } else {
        putchar(tree->val);
    }
}

static struct ASTNode * parse_token(void) {
    struct ASTNode * mem = calloc(sizeof(struct ASTNode), 1);
    assert(mem);

    if (! theres_more_input()) {
        error("input finished");
    }
    char current = *input_pointer;
    switch (current) {
        case '(':
            mem->val = current;
            mem->type = TOKEN_TYPE_L_PAREN;
            input_pointer++;
            return mem;
        case ')':
            mem->val = current;
            mem->type = TOKEN_TYPE_R_PAREN;
            input_pointer++;
            return mem;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            mem->val = current;
            mem->type = TOKEN_TYPE_TERM;
            input_pointer++;
            return mem;
        case '+':
        case '-':
            mem->val = current;
            mem->precedence = 6;
            mem->associativity = LEFT_ASSOC;
            input_pointer++;
            return mem;
        case '*':
        case '/':
        case '%':
            mem->val = current;
            mem->precedence = 7;
            mem->associativity = LEFT_ASSOC;
            input_pointer++;
            return mem;
        case '^':
            mem->val = current;
            mem->precedence = 8;
            mem->associativity = RIGHT_ASSOC;
            input_pointer++;
            return mem;
        default:
            free(mem);
            printf("cannot parse token: %c\n", current);
            exit(EXIT_FAILURE);
            return NULL;
    }
}

static bool stack_has_opers(struct stack_obj * stck) {
    return stck != NULL;
}

static bool stack_oper_comes_before(struct stack_obj * op_stack, struct ASTNode * oper) {
    if (! stack_has_opers(op_stack)) {
        return false;
    }
    struct ASTNode * stack_head = stack_peek(op_stack);
    if (stack_head->type == TOKEN_TYPE_L_PAREN) {
        return false;
    }
    return (stack_head->precedence > oper->precedence)
            || (stack_head->precedence == oper->precedence && stack_head->associativity == LEFT_ASSOC);
}

static void shunting_yard_to_RPN(const char * const input) {
    // parse infix expression, write to stdout
    input_pointer = input;
    static struct stack_obj * oper_stack = NULL; // operators go here

    while (theres_more_input()) {
        struct ASTNode * token = parse_token();
        switch (token->type) {
            case TOKEN_TYPE_TERM:
                putchar(token->val);
                break;
            case TOKEN_TYPE_OPERATOR:
                while (stack_oper_comes_before(oper_stack, token)) {
                    putchar(stack_pop(&oper_stack)->val);
                }
                stack_push(&oper_stack, token);
                break;
            case TOKEN_TYPE_L_PAREN:
                stack_push(&oper_stack, token);
                break;
            case TOKEN_TYPE_R_PAREN:
                while (stack_has_opers(oper_stack) && stack_peek(oper_stack)->type != TOKEN_TYPE_L_PAREN) {
                    putchar(stack_pop(&oper_stack)->val);
                }
                if (stack_has_opers(oper_stack) && stack_peek(oper_stack)->type == TOKEN_TYPE_L_PAREN) {
                    stack_pop(&oper_stack);
                } else {
                    error("mismatched parens");
                }
                break;
        }
    }

    while (stack_has_opers(oper_stack)) {
        putchar(stack_pop(&oper_stack)->val);
    }
    putchar('\n');
}

static struct ASTNode * parse_expression(const char * const input) {
    // shunting yard, returns a parse tree
    struct stack_obj * val_stack = NULL; // values go here
    struct stack_obj * oper_stack = NULL; // operators go here
    input_pointer = input;

    while (theres_more_input()) {
        struct ASTNode * token = parse_token();
        switch (token->type) {
            case TOKEN_TYPE_TERM:
                stack_push(&val_stack, token);
                break;
            case TOKEN_TYPE_OPERATOR:
                while (stack_oper_comes_before(oper_stack, token)) {
                    struct ASTNode * higher_prec_oper = stack_pop(&oper_stack);
                    higher_prec_oper->children_right = stack_pop(&val_stack);
                    higher_prec_oper->children_left = stack_pop(&val_stack);
                    stack_push(&val_stack, higher_prec_oper);
                }
                stack_push(&oper_stack, token);
                break;
            case TOKEN_TYPE_L_PAREN:
                stack_push(&oper_stack, token);
                break;
            case TOKEN_TYPE_R_PAREN:
                while (stack_has_opers(oper_stack) && stack_peek(oper_stack)->type != TOKEN_TYPE_L_PAREN) {
                    struct ASTNode * in_parens_oper = stack_pop(&oper_stack);
                    in_parens_oper->children_right = stack_pop(&val_stack);
                    in_parens_oper->children_left = stack_pop(&val_stack);
                    stack_push(&val_stack, in_parens_oper);
                }
                if (stack_has_opers(oper_stack) && stack_peek(oper_stack)->type == TOKEN_TYPE_L_PAREN) {
                    stack_pop(&oper_stack);
                } else {
                    error("mismatched parens");
                }
                break;
        }
    }

    while (stack_has_opers(oper_stack)) {
        struct ASTNode * oper = stack_pop(&oper_stack);
        oper->children_right = stack_pop(&val_stack);
        oper->children_left = stack_pop(&val_stack);
        stack_push(&val_stack, oper);
    }

    return val_stack->obj;
}

int main(int argc, char * argv[]) {
    if (argc < 2) {
        char input_buf[100];
        fgets(input_buf, 100, stdin);
        shunting_yard_to_RPN(input_buf);
    } else {
        struct ASTNode * tree = parse_expression(argv[1]);
        pretty_print(tree);
        putchar('\n');
    }
}
