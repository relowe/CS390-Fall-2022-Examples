"""
This module contains a lexer for the calc language.

To Write a Lexer
1.) Implement Scanning
    - Scan character by character (Theory requirement)
    - Keep track of line and column number (Practical requirement)
    - implement a function to skip spaces (Practical Requirement)

2.) Implement Tokens
    2.1) Identify the regular portions of our grammar.
         - All terminals are automatically regular. (A -> a)
         - Look out for regular rules:
             A->aA     -or-     A->Aa
    2.2) Separate the tokens out of our parser grammar, and build
         the lexer grammar.
    2.3) Create a representation for all the tokens.
         - be sure to add two "utility" tokens as a practacial 
           requirement: INVALID, EOF

3.) Implement a way to get token details.
    - token: numerical category of the lexed item
    - lexeme: Actual characters that were matched
    - value: numeric value of the lexeme
    - line: Line number where the token begins
    - col: Column where the token begins

    NOTE: Token details are immutable.

4.) Implement the "next" function which consumes and returns
    the next matched token detail structure.
"""
import sys
from enum import Enum,auto
from collections import namedtuple

class Token(Enum):
    '''
    Calculator grammar tokens.
    '''
    EQUAL = auto()
    PLUS  = auto()
    MINUS = auto()
    TIMES = auto()
    DIVIDE = auto()
    POW = auto()
    LPAREN = auto()
    RPAREN = auto()
    VARIABLE = auto()
    INTLIT = auto()
    FLOATLIST = auto()
    INVALID = auto()
    EOF = auto()

TokenDetail = namedtuple('TokenDetail', ('token', 'lexeme', 'value', 'line', 'col'))


class Lexer:
    '''
    The lexer class for the calc language. Converts a text stream
    into a token stream.
    '''

    def __init__(self, lex_file = sys.stdin):
        # set up scanning in our lexer
        self.__lex_file = lex_file
        self.__line = 1
        self.__col = 0
        self.__cur_char = None

        # scan the first character
        self.consume()

        # store the current token
        self.__tok = TokenDetail(Token.INVALID, '', None, 0, 0)


    def consume(self):
        """
        Consumes a character from the stream, and makes it the
        lexer's current character
        """
        self.__cur_char = self.__lex_file.read(1)

        # update position
        self.__col += 1
        if self.__cur_char == '\n':
            self.__col = 0
            self.__line += 1


    def skip_space(self):
        """
        Consume characters until we encounter something other than
        a space
        """
        while self.__cur_char.isspace():
            self.consume()


    def get_char(self):
        """
        Return the current character 
        """
        return str(self.__cur_char)


    def get_line(self):
        """
        Return the current line number
        """
        return self.__line


    def get_col(self):
        """
        Return the current col number
        """
        return self.__col

    def __create_tok(self, token, lexeme=None, value=None, line=None, col=None):
        if not lexeme:
            lexem = self.__cur_char
        if not line:
            line = self.__line
        if not col:
            col = self.__col

        return TokenDetail(token, lexeme, value, line, col)


    def __lex_single(self):
        """
        Recognize group 1 tokens. (Single character tokens which
        are not the prefix of any other token.)
        """

        # handle variables
        if self.__cur_char.isalpha():
            self.__tok = self.__create_tok(Token.VARIABLE)
            self.consume()
            return True

        # handle the fixed single characters
        t = [('=', Token.EQUAL),
             ('+', Token.PLUS),
             ('-', Token.MINUS),
             ('*', Token.TIMES),
             ('/', Token.DIVIDE),
             ('^', Token.POW),
             ('(', Token.LPAREN),
             (')', Token.RPAREN)]
        for tok in t:
            if self.__cur_char == tok[0]:
                self.__tok = self.__create_tok(tok[1])
                self.consume()
                return True

        return False


    def next(self):
        """
        Advance the lexer to the next token and return
        that token.
        """

        #in our language, we skip spaces between tokens
        self.skip_space()

        if self.__lex_single():
            return self.__tok

        # Catch all
        self.__tok = self.__create_tok(Token.INVALID)
        self.consume()

        return self.__tok


if __name__ == '__main__':
    lex = Lexer()
    
    while True:
        print(lex.next())


