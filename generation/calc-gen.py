import random

MAX_RECURSION = 20
depth = 0

def program():
    global depth
    depth = depth + 1

    def rule1():
        return statement()
    def rule2():
        return program() + statement()

    if depth < MAX_RECURSION:
        f = random.choice((rule1, rule2))
    else:
        f = rule1

    result = f()
    depth = depth-1
    return result


def statement():
    f = random.choice((assignment, expression))
    return f() + "\n"


def assignment():
    return variable() + "=" + expression()


def expression():
    global depth
    depth = depth + 1

    def rule1():
        return expression() + "+" + term()
    def rule2():
        return expression() + "-" + term()

    if depth < MAX_RECURSION: 
        f = random.choice((rule1, rule2, term))
    else:
        f = term

    result = f()
    depth = depth-1
    return result


def term():
    global depth
    depth = depth + 1

    def rule1():
        return term() + "+" + factor()
    def rule2():
        return term() + "-" + factor()

    if depth < MAX_RECURSION:
        f = random.choice((rule1, rule2, factor))
    else:
        f = factor

    result = f()
    depth = depth-1
    return result


def factor():
    global depth
    depth = depth + 1

    def rule1():
        return exponent() + "+" + factor()

    if depth < MAX_RECURSION:
        f = random.choice((rule1, exponent))
    else:
        f = exponent

    result = f()
    depth = depth-1
    return result


def exponent():
    def rule1():
        return "(" + expression() + ")"

    f = random.choice((rule1, variable, number))
    return f()


def variable():
    l = random.randint(0, 25)
    l = l + ord('A')
    return chr(l)


def number():
    f = random.choice((integer_literal, float_literal))
    return f()


def integer_literal():
    global depth

    depth = depth + 1

    if depth >= MAX_RECURSION and random.choice((True, False)):
        depth = depth - 1
        return str(random.randint(0, 9))
    else:
        result = integer_literal() + str(random.randint(0,9))
        depth = depth - 1
        return result


def float_literal():
    if random.choice((True, False)):
        return integer_literal() + "." + integer_literal()
    else:
        return "." + integer_literal()

# to generate a string, we will call the start symbol
if __name__ == "__main__":
    print(program())
