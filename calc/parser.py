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
    NEG = auto()

ariness = { ParseType.ATOMIC: 0, ParseType.INPUT: 1, 
            ParseType.ASSIGN: 2, ParseType.ADD: 2, 
            ParseType.SUB: 2, ParseType.MUL: 2, 
            ParseType.DIV: 2, ParseType.POW: 2 , ParseType.NEG: 1}


class ParseTree:
    def __init__(self, node_type=ParseType.PROGRAM, token=None):
        self.node_type = node_type
        self.children = []
        self.token = token

    def print(self, level=0):
        """
        A handy print method. (for debugging) 
        """

        # get the midpoint
        m = int(len(self.children)/2)-1

        # left half
        for c in self.children[-1:m:-1]:
            c.print(level + 2)

        # print our node
        indent = ' '*level

        if self.node_type == ParseType.ATOMIC:
            print(indent, self.token.lexeme, sep='')
        else:
            print(indent, self.node_type.name, sep='')

        # right half
        for c in self.children[m::-1]:
            c.print(level+2)


    def insert_left_leaf(self, leaf):
        """
        Insert at the extreme left leaf position.
        """
        if len(self.children) < ariness[self.node_type]:
            self.children.insert(0, leaf)
            return 

        self.children[0].insert_left_leaf(leaf)
        

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

        tree = ParseTree(ParseType.PROGRAM, self.__lexer.get_tok())

        while not self.__has(Token.EOF):
            tree.children.append(self.__statement())
        return tree


    def __statement(self):
        if self.__has(Token.VARIABLE):
            leaf = self.__lexer.get_tok()
            self.__next()
            left = ParseTree(ParseType.ATOMIC, token=leaf)

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
            node = ParseTree(ParseType.ASSIGN, self.__lexer.get_tok())
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
            node.insert_left_leaf(left)
        else:
            node = left
        return node


    def __expression2(self):
        if self.__has(Token.PLUS):
            self.__next()
            node = ParseTree(ParseType.ADD, self.__lexer.get_tok())
            t = self.__term()
            e = self.__expression2()
            node.children.append(t)
            if e:
                e.insert_left_leaf(node)
                node = e
            return node
        elif self.__has(Token.MINUS):
            self.__next()
            node = ParseTree(ParseType.SUB, self.__lexer.get_tok())
            t = self.__term()
            e = self.__expression2()
            node.children.append(t)
            if e:
                e.insert_left_leaf(node)
                node = e
            return node
        else:
            return False

    def __term(self):
        left = self.__factor()
        node = self.__term2()
        if node:
            node.insert_left_leaf(left)
        else:
            node = left
        return node


    def __term2(self):
        if self.__has(Token.TIMES):
            self.__next()

            node = ParseTree(ParseType.MUL, self.__lexer.get_tok())
            f = self.__factor()
            t = self.__term2()
            node.children.append(f)

            if t:
                t.insert_left_leaf(node)
                node = t
            return node

        elif self.__has(Token.DIVIDE):
            self.__next()

            node = ParseTree(ParseType.DIV, self.__lexer.get_tok())
            f = self.__factor()
            t = self.__term2()
            node.children.append(f)

            if t:
                t.insert_left_leaf(node)
                node = t
            return node

        else: 
            return False


    def __factor(self):
        if self.__has(Token.MINUS):
            left = ParseTree(ParseType.NEG, self.__lexer.get_tok())
            left.children.append(self.__exponent())
        else:
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

            node = ParseTree(ParseType.POW, self.__lexer.get_tok())
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
            node = ParseTree(ParseType.ATOMIC, token=leaf)
            return node


    def __input(self):
        self.__must_be(Token.INPUT)
        self.__next()


        self.__must_be(Token.VARIABLE)

        # build the node
        node = ParseTree(ParseType.INPUT, self.__lexer.get_tok())
        leaf = self.__lexer.get_tok()
        left = ParseTree(ParseType.ATOMIC, leaf)
        node.children.append(left)

        self.__next()
        return node


# unit test 
if __name__ == "__main__":
    p = Parser(Lexer())
    tree = p.parse()
    tree.print()
