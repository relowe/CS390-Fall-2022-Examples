"""
The Calc language interpreter. The semantics of calc are as follows:
    - Math operators: +, -, *, /, **, ^
    - Parenthesis ()
    - All operations follow the usual order of operations.
    - Calc always does floating point arithmetic.
    - input can read a variable from the user.
      input x

      Prompt: "x="
    - Assignment is performed by "="
       x = 2
    - Any statement other than input or an assignment prints the
      result to the screen.

      x=2
      y=1
      x+y     ... print 3
Errors
    - identify context sensitive errors
    - Variables must be assigned before they are read.
"""
from parser import Parser, ParseType
from lexer import Token, Lexer
import sys

# the variables for the calculator
var = {}


def eval_parse_tree(t):
    """
    Ealuate the given parse tree.
    """
    if t.node_type == ParseType.PROGRAM:
        return eval_program(t)
    elif t.node_type == ParseType.ATOMIC:
        return eval_atomic(t)
    elif t.node_type == ParseType.INPUT:
        return eval_input(t)
    elif t.node_type == ParseType.PRINT:
        return eval_print(t)
    elif t.node_type == ParseType.ASSIGN:
        return eval_assign(t)
    elif t.node_type == ParseType.ADD:
        return eval_add(t)
    elif t.node_type == ParseType.SUB:
        return eval_sub(t)
    elif t.node_type == ParseType.MUL:
        return eval_mul(t)
    elif t.node_type == ParseType.DIV:
        return eval_div(t)
    elif t.node_type == ParseType.POW:
        return eval_pow(t)
    elif t.node_type == ParseType.NEG:
        return eval_neg(t)
    elif t.nod_type in (ParseType.IF, ParseType.IFELSE):
        return eval_branch(t)


def eval_program(t):
    """
    Evaluate the program
    """

    for c in t.children:
        result = eval_parse_tree(c)

        # print the result if it is not assign or input
        if c.node_type not in (ParseType.INPUT, ParseType.ASSIGN):
            print(result)

def eval_atomic(t):
    """
    Evaluate the atomic value.
    """

    # get literals
    if t.token.token in (Token.INTLIT, Token.FLOATLIT):
        return t.token.value
    
    # get the variable
    if t.token.lexeme not in var:
        print(f"Undefined variable {t.token.lexeme} on line {t.token.line}")
        sys.exit(-1)
    return var[t.token.lexeme]


def eval_input(t):
    """
    Evaluate an input statement
    """
    global var

    # get the variable we are going to write
    v = t.children[0].token.lexeme

    var[v] = float(input(f"{v}="))


def eval_print(t):
    """
    Evaluate a print statement
    """
    print(t.children[0].token.value)


def eval_assign(t):
    """
    Evaluate an assignment statement
    """
    global var 

    # get the variable we are going to write
    v = t.children[0].token.lexeme

    # evaluate the expression and assign the result
    var[v] = eval_parse_tree(t.children[1])


def eval_add(t):
    """
    Evaluate an addition operation.
    """
    return eval_parse_tree(t.children[0]) + eval_parse_tree(t.children[1])


def eval_sub(t):
    """
    Evaluate an subtraction operation.
    """
    return eval_parse_tree(t.children[0]) - eval_parse_tree(t.children[1])

def eval_mul(t):
    """
    Evaluate an multiplication operation.
    """
    return eval_parse_tree(t.children[0]) * eval_parse_tree(t.children[1])

def eval_div(t):
    """
    Evaluate an multiplication operation.
    """
    left = eval_parse_tree(t.children[0])
    right = eval_parse_tree(t.children[1])
    if right == 0:
        print(f"Division by 0 on line {t.token.line}")
        sys.exit(-1)
    return left/right

def eval_pow(t):
    """
    Evaluate an exponent operation.
    """
    return eval_parse_tree(t.children[0]) ** eval_parse_tree(t.children[1])


def eval_neg(t):
    """
    Evaluate a negation
    """
    return -eval_parse_tree(t.children[0])


def eval_branch(t):
    """
    Evaluate a branch
    """

    if eval_tree(t.children[0]):
        eval_tree(t.children[1])
    elif t.node_type == ParseType.IFELSE:
        eval_tree(t.children[2])

if __name__ == "__main__":
    if len(sys.argv) == 2:
        f = open(sys.argv[1])
        l = Lexer(f)
    else:
        l = Lexer()
    parser = Parser(l)
    pt = parser.parse()
    eval_parse_tree(pt)
