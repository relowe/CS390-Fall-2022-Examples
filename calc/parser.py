"""
This is a recursive descent parser for the calc
language. 

To write a parser:

    1.) Construct the basic interface (lexer, next, has, must_be).
    2.) Convert each BNF rule into a mutually recursive function.
    3.) Add data structures to build the parse tree.

To construct a parse tree:
    1.) Identify the tree node types. (atomic or actions)
    2.) Create a data structure which can represent trees with
        arbitrary numbers of children.
    3.) Modify each of the recursive descent functions to return
        a parse tree.
"""
import sys
from enum import Enum, auto
from lexer import Token,Lexer

class ParseType(Enum):
    PROGRAM = auto()
    ATOMIC = auto()
    INPUT = auto()
    ASSIGN = auto()
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    POW = auto()


class ParseTree:
    def __init__(self, node_type=ParseType.PROGRAM):
        self.node_type = node_type
        self.children = []

class Parser:
    """
    Parser state will follow the lexer state.
    We consume the stream token by token.
    Match our tokens, if no match is possible, 
    print an error and stop parsing.
    """

    def __init__(self, lexer): 
        self.__lexer = lexer


    def __next(self):
        """
        Advance the lexer.
        """
        self.__lexer.next()


    def __has(self, t):
        """
        Return true if t matches the current token.
        """
        ct = self.__lexer.get_tok()
        return ct.token == t


    def __must_be(self, t):
        """
        Return true if t matches the current token.
        Otherwise, we print an error message and
        exit.
        """
        if self.__has(t):
            return True

        # print an error
        ct = self.__lexer.get_tok()
        print(f"Parser error at line {ct.line}, column {ct.col}.\nReceived token {ct.token.name} expected {t.name}")
        sys.exit(-1)

    def parse(self):
        """
        Attempt to parse a program.
        """
        return self.__program()

    ###########
    # From here on down, everything is calc specific
    ###########

    def __program(self):
        self.__next()

        tree = ParseTree(ParseType.PROGRAM)

        while not self.__has(Token.EOF):
            tree.children.append(self.__statement())
        return tree


    def __statement(self):
        if self.__has(Token.VARIABLE):
            leaf = self.__lexer.get_tok()
            self.__next()
            left = ParseTree(ParseType.ATOMIC)
            left.children.append(leaf)

            node = self.__ao_expression(left)
            return node
        elif self.__has(Token.LPAREN):
            self.__next()
            node = self.__expression()
            self.__must_be(Token.RPAREN)
            self.__next()

            return node
        elif self.__has(Token.INPUT):
            return self.__input()
        else:
            return self.__expression()


    def __ao_expression(self, lv):
        if self.__has(Token.EQUAL):
            self.__next()
            node = ParseTree(ParseType.ASSIGN)
            node.children.append(lv)
            node.children.append(self.__expression())
            return node

        else:
            node = lv
            left1 = self.__factor2()
            if left1:
                left1.children.insert(0, node)
                node = left1
            left2 = self.__term2()
            if left2:
                left2.children.insert(0, node)
                node = left2
            left3 = self.__expression2()
            if left3:
                left3.children.insert(0, node)
                node = left3
            return node


    def __expression(self):
        left = self.__term()
        node = self.__expression2()
        if node:
            node.children.insert(0, left)
        else:
            node = left
        return node


    def __expression2(self):
        if self.__has(Token.PLUS):
            self.__next()
            node = ParseTree(ParseType.ADD)
            right = self.__term()
            right2 = self.__expression2()
            if right2:
                right2.children.insert(0, right)
                right = right2
            node.children.append(right)
            return node
        elif self.__has(Token.MINUS):
            self.__next()
            node = ParseTree(ParseType.SUB)
            right = self.__term()
            right2 = self.__expression2()
            if right2:
                right2.children.insert(0, right)
                right = right2
            node.children.append(right)
            return node
        else:
            return False

    def __term(self):
        left = self.__factor()
        node = self.__term2()
        if node:
            node.children.insert(0, left)
        else:
            node = left
        return node


    def __term2(self):
        if self.__has(Token.TIMES):
            self.__next()

            node = ParseTree(ParseType.MUL)
            right = self.__factor()
            right2 = self.__term2()
            if right2:
                right2.children.insert(0,right)
                right = right2
            node.children.append(right)
            return node

        elif self.__has(Token.DIVIDE):
            self.__next()

            node = ParseTree(ParseType.DIV)
            right = self.__factor()
            right2 = self.__term2()
            if right2:
                right2.children.insert(0,right)
                right = right2
            node.children.append(right)
            return node

        else: 
            return False


    def __factor(self):
        left = self.__exponent()
        node = self.__factor2()
        if node:
            node.children.insert(0, left)
        else:
            node = left
        return node


    def __factor2(self):
        if self.__has(Token.POW):
            self.__next()

            node = ParseTree(ParseType.POW)
            node.children.append(self.__factor())
            return node
        else:
            return False


    def __exponent(self):
        if self.__has(Token.LPAREN):
            self.__next()
            node = self.__expression()
            self.__must_be(Token.RPAREN)
            self.__next()
            return node
        elif self.__has(Token.VARIABLE) or self.__has(Token.INTLIT) or self.__must_be(Token.FLOATLIT):
            leaf = self.__lexer.get_tok() 
            self.__next()
            node = ParseTree(ParseType.ATOMIC)
            node.children.append(leaf)


    def __input(self):
        self.__must_be(Token.INPUT)
        self.__next()


        self.__must_be(Token.VARIABLE)

        # build the node
        node = ParseTree(ParseType.INPUT)
        leaf = self.__lexer.get_tok()
        left = ParseTree(ParseType.ATOMIC)
        left.children.append(leaf)
        node.children.append(left)

        self.__next()
        return node


# unit test 
if __name__ == "__main__":
    p = Parser(Lexer())
    tree = p.parse()
    print(tree.children)
