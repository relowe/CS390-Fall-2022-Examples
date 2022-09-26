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

    Group Tokens into the following categories:
    1.) Single character tokens which are not the prefix of any other
        token.
    2.) Multiple-Character tokens where each token is of fixed length
        and there may be common prefixes. (The token shares no 
        prefix in common with a group 3 token.)
    3.) Everything else (variable width)
        - usually requires a customized approach
        - Consume until an inconsistency is reached.
        - Match any fixed tokens
        - Implementing a finite state machine
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
    FLOATLIT = auto()
    INPUT = auto()
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

    def get_tok(self):
        return self.__tok

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
        # handle the fixed single characters
        t = [('=', Token.EQUAL),
             ('+', Token.PLUS),
             ('-', Token.MINUS),
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

    def __lex_multi_fixed(self):
        """
        Attempt to match multi-character tokens which may overlap in
        prefix.
        """
        t = [ ('*', Token.TIMES), ('**', Token.POW) ]
        
        # Edit the above ^^^^^^   Only that. (Yes really)
        # when doing your homework, you should not need to change any
        # of the rest of the code in this function

        # accumulate characters until there is only one possibility 
        cur_lex = ""
        line = self.__line
        col = self.__col
        while len(t) > 1:
            # try the next character
            trial_lex = cur_lex + self.__cur_char

            # find the consistent tokens
            t_old = t
            t = [ tok for tok in t if tok[0].startswith(trial_lex) ]

            # stop if we have eliminated all the tokens
            if len(t) == 0:
                t = t_old
                break
            else:
                cur_lex = trial_lex
                self.consume()

        # handle no match
        if len(cur_lex) == 0:
            return False

        # find an exact match in the tokens
        t = [ tok for tok in t if tok[0] == cur_lex ]

        if len(t) < 1:
            # incomplete token
            self.__tok = self.__create_tok(Token.INVALID, lexeme = cur_lex, line=line, col=col)
        else:
            # token found
            self.__tok = self.__create_tok(t[0][1], lexeme = cur_lex, line=line, col=col)

        return True


    def __lex_other(self):
        # narrow things down by first character
        if self.__cur_char.isdigit() or self.__cur_char == ".":
            return self.__lex_number()
        elif self.__cur_char.isalpha() or self.__cur_char == "_":
            return self.__lex_keyword_or_var()
        return False


    def __lex_number(self):
        # preserve where things begin
        cur_lex = ""
        line = self.__line
        col = self.__col

        # consume the leading digits 
        while self.__cur_char.isdigit():
            cur_lex += self.__cur_char
            self.consume()
        
        # assume we have an integer
        t = Token.INTLIT

        # check to see if we proceed
        if self.__cur_char == ".":
            t = Token.FLOATLIT
            cur_lex += self.__cur_char
            self.consume()
            while self.__cur_char.isdigit():
                cur_lex += self.__cur_char
                self.consume()

        # invalid check
        if cur_lex[-1] == '.':
            t = Token.INVALID

        # construct the value
        if t == Token.INTLIT:
            v = int(cur_lex)
        elif t == Token.FLOATLIT:
            v = float(cur_lex)
        else:
            v = None

        #construct the token 
        self.__tok = self.__create_tok(t, cur_lex, v, line, col)
        return True


    def __lex_keyword_or_var(self):
        kw = [ ('input', Token.INPUT) ]

        # ^^^^^ Modify only the keyword list to use this function ^^^^^^^
        
        # start things off
        cur_lex = ''
        line = self.__line
        col = self.__col

        # accumulate all consistent characters
        while self.__cur_char.isalpha() or self.__cur_char.isdigit() or self.__cur_char == '_':
            cur_lex += self.__cur_char
            self.consume()

        # check if it's a keyword
        kw = [ tok for tok in kw if tok[0] == cur_lex ]
        if len(kw) == 1:
            t = kw[0][1]
        else:
            t = Token.VARIABLE

        self.__tok = self.__create_tok(t, cur_lex, line=line, col=col)
        return True


    def next(self):
        """
        Advance the lexer to the next token and return
        that token.
        """

        #in our language, we skip spaces between tokens
        self.skip_space()

        # detect end of file
        if not self.__cur_char:
            self.__tok = self.__create_tok(Token.EOF)
            return self.__tok
        elif self.__lex_single():
            return self.__tok
        elif self.__lex_multi_fixed():
            return self.__tok
        elif self.__lex_other():
            return self.__tok

        # Catch all
        self.__tok = self.__create_tok(Token.INVALID)
        self.consume()

        return self.__tok


if __name__ == '__main__':
    lex = Lexer()
    
    while lex.get_tok().token != Token.EOF:
        print(lex.next())


