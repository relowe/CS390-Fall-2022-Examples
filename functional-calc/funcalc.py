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
from parser import Parser, ParseType, ParseTree
from lexer import Token, Lexer
import sys
from enum import Enum, auto
from collections import ChainMap, namedtuple

class RefType(Enum):
    VARIABLE = auto()
    FUNCTION = auto()
    CLOSURE = auto()


class Ref:
    """
    Reference which is bound to a name.
    """
    def __init__(self, ref_type, ref_value):
        self.ref_type = ref_type
        self.ref_value = ref_value


class RefEnv:
    def __init__(self, parent=None):
        self.tab = ChainMap()
        if parent:
            self.tab = ChainMap(self.tab, parent.tab)
        self.return_value = None

    def lookup(self, name):
        """
        Search for a symbol in the reference environment.
        """
        # try to find the symbol
        if name not in self.tab:
            return None

        return self.tab[name]

    def insert(self, name, ref):
        """
        Insert a symbol into the inner-most reference environment.
        """
        self.tab[name] = ref

Closure = namedtuple('Closure', ('function', 'env'))


def eval_parse_tree(t, env):
    """
    Ealuate the given parse tree.
    """
    if t.node_type == ParseType.PROGRAM:
        return eval_program(t, env)
    elif t.node_type == ParseType.ATOMIC:
        return eval_atomic(t, env)
    elif t.node_type == ParseType.INPUT:
        return eval_input(t, env)
    elif t.node_type == ParseType.PRINT:
        return eval_print(t, env)
    elif t.node_type == ParseType.ASSIGN:
        return eval_assign(t, env)
    elif t.node_type == ParseType.ADD:
        return eval_add(t, env)
    elif t.node_type == ParseType.SUB:
        return eval_sub(t, env)
    elif t.node_type == ParseType.MUL:
        return eval_mul(t, env)
    elif t.node_type == ParseType.DIV:
        return eval_div(t, env)
    elif t.node_type == ParseType.POW:
        return eval_pow(t, env)
    elif t.node_type == ParseType.NEG:
        return eval_neg(t, env)
    elif t.node_type in (ParseType.IF, ParseType.IFELSE):
        return eval_branch(t, env)
    elif t.node_type == ParseType.LT:
        return eval_lt(t, env)
    elif t.node_type == ParseType.ET:
        return eval_et(t, env)
    elif t.node_type == ParseType.DEF:
        return eval_def(t, env)
    elif t.node_type == ParseType.CALL:
        return eval_call(t, env)
    elif t.node_type == ParseType.RETURN:
        return eval_return(t, env)


def eval_program(t, env):
    """
    Evaluate the program
    """
    
    fun_result = None
    for c in t.children:
        result = eval_parse_tree(c, env)

        # remember any non-none result
        if result is not None:
            fun_result = result

        # check to see if we have returned
        if env.return_value is not None:
            return env.return_value
    return fun_result



def eval_atomic(t, env):
    """
    Evaluate the atomic value.
    """

    # get literals
    if t.token.token in (Token.INTLIT, Token.FLOATLIT, Token.STRING):
        return t.token.value
    
    # get the variable
    v = env.lookup(t.token.lexeme)
    if not v:
        print(f"Undefined variable {t.token.lexeme} on line {t.token.line}")
        sys.exit(-1)

    if v.ref_type == RefType.FUNCTION:
        return Closure(v.ref_value, env)
    else:
        return v.ref_value


def bind(env, name, ref):
    """
    Bind the name in env to ref according to the correct scope
    resolution rules.
    """
    v = env.lookup(env)
    if v:
        # rebind to an existing name
        v.ref_value = ref.ref_value
        v.ref_type = ref.ref_type
    else:
        env.insert(name, ref)


def eval_input(t, env):
    """
    Evaluate an input statement
    """
    global var

    # get the variable we are going to write
    v = t.children[0].token.lexeme

    bind(env, v, Ref(RefType.VARIABLE, float(input(f"{v}="))))


def eval_print(t, env):
    """
    Evaluate a print statement
    """
    print(eval_parse_tree(t.children[0], env))


def eval_assign(t, env):
    """
    Evaluate an assignment statement
    """
    global var 

    # get the variable we are going to write
    v = t.children[0].token.lexeme

    # evaluate the expression and assign the result, env
    val = eval_parse_tree(t.children[1], env)

    ref_type = RefType.VARIABLE
    if type(val) == Closure:
        ref_type = RefType.CLOSURE
    bind(env, v, Ref(ref_type, val))


def eval_add(t, env):
    """
    Evaluate an addition operation.
    """
    return eval_parse_tree(t.children[0], env) + eval_parse_tree(t.children[1], env)


def eval_sub(t, env):
    """
    Evaluate an subtraction operation.
    """
    return eval_parse_tree(t.children[0], env) - eval_parse_tree(t.children[1], env)

def eval_mul(t, env):
    """
    Evaluate an multiplication operation.
    """
    return eval_parse_tree(t.children[0], env) * eval_parse_tree(t.children[1], env)

def eval_div(t, env):
    """
    Evaluate an multiplication operation.
    """
    left = eval_parse_tree(t.children[0], env)
    right = eval_parse_tree(t.children[1], env)
    if right == 0:
        print(f"Division by 0 on line {t.token.line}")
        sys.exit(-1)
    return left/right

def eval_pow(t, env):
    """
    Evaluate an exponent operation.
    """
    return eval_parse_tree(t.children[0], env) ** eval_parse_tree(t.children[1], env)


def eval_neg(t, env):
    """
    Evaluate a negation
    """
    return -eval_parse_tree(t.children[0], env)


def eval_branch(t, env):
    """
    Evaluate a branch
    """
    if eval_parse_tree(t.children[0], env):
        eval_parse_tree(t.children[1], env)
    elif t.node_type == ParseType.IFELSE:
        eval_parse_tree(t.children[2], env)


def eval_lt(t, env):
    """
    Evaluate < operation
    """
    return eval_parse_tree(t.children[0], env) < eval_parse_tree(t.children[1], env) 


def eval_et(t, env):
    """
    Evaluate < operation
    """
    return eval_parse_tree(t.children[0], env) == eval_parse_tree(t.children[1], env) 


def eval_def(t, env):
    """
    Define a function
    """

    # functions are always local (by design)
    name = t.token.lexeme
    env.insert(name, Ref(RefType.FUNCTION, t))


def eval_call(t, env):
    """
    Call a function
    """
    name = t.children[0].token.lexeme
    arglist = t.children[1]

    # retrieve the function
    fun = env.lookup(name)
    if not fun:
        print(f"Call to undefined function {name} on line {t.token.line}")
        sys.exit(-1)
    elif fun.ref_type not in (RefType.FUNCTION, RefType.CLOSURE):
        print(f"Call to non-function {name} on line {t.token.line}")
        sys.exit(-1)

    if fun.ref_type == RefType.CLOSURE:
        # closure call
        local = fun.ref_value.env
        fun = fun.ref_value.function
    else:
        # get the function
        fun = fun.ref_value

        # create the local environment
        local = RefEnv(env)

    # get the parameter list
    paramlist = fun.children[0]

    # verify the parameter list
    if len(arglist.children) != len(paramlist.children):
        print(f"Wrong number of parameters to function {name} on line {t.token.line}")
        sys.exit(-1)


    # all parameters are local (by design)
    for i in range(len(paramlist.children)):
        local.insert(paramlist.children[i].token.lexeme, 
                     Ref(RefType.VARIABLE,
                         eval_parse_tree(arglist.children[i], env)))

    # call the function
    return eval_parse_tree(fun.children[1], local)


def eval_return(t, env):
    """
    Evaluate Return
    """
    env.return_value = eval_parse_tree(t.children[0], env)
    return env.return_value



if __name__ == "__main__":
    if len(sys.argv) == 2:
        f = open(sys.argv[1])
        l = Lexer(f)
    else:
        l = Lexer()
    parser = Parser(l)
    pt = parser.parse()
    eval_parse_tree(pt, RefEnv())


