#!usr/bin/python3
# -*- coding: utf-8 -*-

# This python prog is an attempt to make a PASCAL lang interpreter
# POINTS TO REMEMBER:
# The smallest unit of any program is called a token
# A Token object has a "type" and a "value" associated with it
# So the interpreter breaks the program(input string) into tokens through a tokenizer (lexical analyzer) 
# The interpreter will have a consume method ("eat") which compares the current token with a token type
# and if they match then, it "consumes" the token and gets the next token 
# Also, the interpreter will have an execute method which executes (evaluates) the input string

# Program Task Flow: 
# - [x] Interpret the arithmetic expression INTEGER -> ADD -> INTEGER (single digit integer)
# - [x] Add whitespace allowance (Eg. 3 + 5 is also valid)
# - [x] Interpret the arithmetic expression INTEGER -> ADD | SUB -> INTEGER (multiple digit integer) 
# - [x] Add chaining in the expressions (Eg. 3 + 5 - 4 + 2)
# - [x] Add multiplication and division
# - [x] Add mixing of operators (Eg. 3 - 5 * 2 ^ 2)
# - [x] Add parenthesis (Eg. (3 - 5) * 2)


# Grammar
# 1) Chaining + and - operators (eg. 4 - 5 + 3 - 7) 
################################################################
#   expr : term ((ADD | SUB) term)*                            #                       
#   term : INTEGER                                             #                              
#                                                              # 
################################################################

# 2) Chaining all the operators together (eg. 7 - 5 * 2 ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            # 
#   term : factor ((MUL | DIV) factor)*                        # 
#   factor : base (RAISETO factor)*                            # 
#   base : INTEGER                                             # 
#                                                              #     
################################################################

# 3) Chaining with parenthesis (eg. 7 - (5 * 2) ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            #     
#   term : factor ((MUL | DIV ) factor)*                       # 
#   factor : base (RAISETO factor)*                            #             
#   base: INTEGER | \( expr \)                                 #             
################################################################

INTEGER, ADD, SUB, MUL, DIV, EXP, LPAREN, RPAREN, EOF = ("INTEGER", "ADD", "SUB", "MUL", "DIV",
        "EXP", "LPAREN", "RPAREN" ,"EOF")
VALID_OPERS = {
                "+": ADD,
                "-": SUB,
                "*": MUL,
                "/": DIV,
                "^": EXP
            }

PARENS = {
            "(": LPAREN,
            ")": RPAREN
        }


class Token:
    """
    Token Types:

    INTEGER, ADD, EOF (end of file/input)
    """
    def __init__(self, type, val):
        # INTEGER, ADD, EOF
        self.type = type
        # token value: 0,1,2,3,4,5,6,7,8,9,'+','-' or None
        self.val = val

    def __str__(self):
        """
        String representation of the class Token
        """
        return "Token<{type}, {value}>".format(type=self.type, value=repr(self.val))

    def __repr__(self):
        return self.__str__()

class Lexer:
    """
    The Lexer class for parsing analyzing the string and building tokens from it
    """
    def __init__(self, input_str):
        self._input_str = input_str
        self._pos = 0
        self.curr_char = self._input_str[self._pos]

    def _error(self) -> None:
        raise Exception("Error parsing the input")

    # advance through the input_str
    def _advance(self) -> None:
        self._pos += 1
        if self._pos > len(self._input_str) - 1:
            self.curr_char = None # indicated the end of input
        # move fwd in the input str
        else:
            self.curr_char = self._input_str[self._pos]

    def _ign_whitespaces(self) -> None:
        """
        Ignores the whitespaces in the input string
        """
        while self.curr_char is not None and self.curr_char.isspace():
            self._advance()

    def _read_int(self):
        """
        If lexer encounters a digit, this checks if it is a multi digit int
        """
        res_int: str = ""
        while self.curr_char is not None and self.curr_char.isdigit():
            res_int += self.curr_char
            self._advance()
        return res_int

    # Build the next token from the current char
    def _nxt_token(self) -> Token:
        """
        Returns the next token by reading the current char
        """
        while self.curr_char is not None:

            if self.curr_char.isspace():
                self._ign_whitespaces()
                continue

            # isdigit or an operator
            if self.curr_char.isdigit():
                dig_token = Token(INTEGER, int(self._read_int()))
                return dig_token

            # lookup the VALID_OPERS dict for the current char
            # VALID_OPERS[curr_char] gives the type of operator
            if self.curr_char in list(VALID_OPERS.keys()):
                opr_token = Token(VALID_OPERS[self.curr_char], self.curr_char)
                self._advance()
                return opr_token

            if self.curr_char in list(PARENS.keys()):
                paren_token = Token(PARENS[self.curr_char], self.curr_char)
                self._advance()
                return paren_token

            # if we reach here, parsing error occurred
            self._error()

        # if current char becomes None, EOF occurred
        return Token(EOF, None)


class Interpreter:
    """
    Class attributes:
    self.lexer : The Lexer instance to avoid crowding in this class
    self._curr_token : The current token parsed (Token)

    Methods:
    
    self._error : When any parsing error occurs
    """
    def __init__(self, lexer):
        self.lexer = lexer
        # set the current token via lexer's next token method
        self._curr_token: Token = self.lexer._nxt_token()
  
    def _error(self):
        raise Exception("Invalid syntax")
    
    def consume(self, token_type) -> None:
        """
        If current token type equals the token_type, "consume" and get/set the next token
        """
        if self._curr_token.type == token_type:
            self._curr_token = self.lexer._nxt_token()

        # if token type doesnot match, raise exception
        else:
            self._error()

    def _base(self):
        """
        The Base defined in grammar of chaining (no. 2)
        """
        token = self._curr_token

        if token.type == INTEGER:
            self.consume(self._curr_token.type)
            
            # take a peek at the next token...
            # if two integer tokens are side by side without any operator between them, thats a syntax error
            nxt_tok = self._curr_token
            if nxt_tok.type == INTEGER:
                self._error()
            
            return token.val
        elif token.type == LPAREN:
            self.consume(LPAREN)
            # call expr for calculating the expression inside both the parenthesis
            res = self.expr()
            # after the expression is done calculating, the right parenthesis will only be present
            # as no other method is checking for the right paren token type
            self.consume(RPAREN)
            # return the result at last
            return res
        
    def _factor(self):
        """
        The factor as defined in the grammar of chaining (no. 2)
        """
        # result from calling the base method
        res = self._base()

        while self._curr_token.type == EXP:
            self.consume(self._curr_token.type)
            # call factor recursively, Eg. 3^3^3
            # it evals to 3^27
            res = pow(res, self._factor())

        return res


    def _term(self):
        """
        The term as defined in the grammar of chaining (no. 2)
        """
        # result from calling the factor method
        res_fac = self._factor()

        while self._curr_token.type in (MUL, DIV):
            if self._curr_token.type == MUL:
                # calc the result by calling the next factor as done below
                self.consume(self._curr_token.type)
                res_fac *= self._factor()
            elif self._curr_token.type == DIV:
                self.consume(self._curr_token.type)
                res_fac /= self._factor()
        
        return res_fac


    # Start evaluating
    def expr(self):
        """
        Evaluates the expression, from the tokens
        """
        # finally get the term
        res = self._term()

        # loop untill current token is any operator and add that to result
        while self._curr_token.type in (ADD, SUB):
            # consume the token and add the next term to result
            if self._curr_token.type == ADD:
                self.consume(self._curr_token.type)
                res += self._term()
            elif self._curr_token.type == SUB:
                self.consume(self._curr_token.type)
                res -= self._term()


        # return the res, once all the input is parsed
        return res


def main():
    while True:
        try:
            input_str = input("calc> ")
        except EOFError:
            print("Unexpected EOF when reading the input\n")
            break
        except KeyboardInterrupt:
            print("\nExiting the program...\n")
            break

        # when input_str is not provided
        if not input_str:
            continue

        lexer = Lexer(input_str)
        interp = Interpreter(lexer)
        res = interp.expr()
        print(res)

if __name__ == "__main__":
    main()
