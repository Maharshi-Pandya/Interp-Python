#!usr/bin/python3.9
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
# - [x] Interpret the arithmetic expression NUMBER -> ADD -> NUMBER (single digit integer)
# - [x] Add whitespace allowance (Eg. 3 + 5 is also valid)
# - [x] Interpret the arithmetic expression NUMBER -> ADD | SUB -> NUMBER (multiple digit integer) 
# - [x] Add chaining in the expressions (Eg. 3 + 5 - 4 + 2)
# - [x] Add multiplication and division
# - [x] Add mixing of operators (Eg. 3 - 5 * 2 ^ 2)
# - [x] Add parenthesis (Eg. (3 - 5) * 2)
# - [x] Add floating point nums

# Grammar
# 1) Chaining + and - operators (eg. 4 - 5 + 3 - 7) 
################################################################
#   expr : term ((ADD | SUB) term)*                            #                       
#   term : NUMBER                                             #                              
#                                                              # 
################################################################

# 2) Chaining all the operators together (eg. 7 - 5 * 2 ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            # 
#   term : factor ((MUL | DIV) factor)*                        # 
#   factor : base (RAISETO factor)*                            # 
#   base : NUMBER                                             # 
#                                                              #     
################################################################

# 3) Chaining with parenthesis (eg. 7 - (5 * 2) ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            #     
#   term : factor ((MUL | DIV ) factor)*                       # 
#   factor : base (RAISETO factor)*                            #             
#   base: NUMBER | \( expr \)                                 #             
################################################################

import re
import operator   

NUMBER, ADD, SUB, MUL, DIV, EXP, LPAREN, RPAREN, EOF = ("NUMBER", "ADD", "SUB", "MUL", "DIV",
        "EXP", "LPAREN", "RPAREN" ,"EOF")

TOKEN_EXPRS = [
            # for white spaces
            (r"[ \n\t]+", None),
            (r"\(", LPAREN),
            (r"\)", RPAREN),
            (r"\*\*", EXP),
            (r"/", DIV),
            (r"\*", MUL),
            (r"\+", ADD),
            (r"-", SUB),
            # ints and floats pattern
            (r"[0-9]*\.?[0-9]+", NUMBER),
        ]

# will be handy when breaking down into terms, factors, bases etc...
opr_funcs = {
            "**": operator.pow,
            "/": operator.truediv,
            "*": operator.mul,
            "+": operator.add,
            "-": operator.sub
        }


class Token:
    """
    Token attrs:
    
    type: The token tag (eg. NUMBER, ADD, SUB etc...)
    val:
        - Numerical value for ints and floats
        - operator function for operators
    """
    def __init__(self, type, val):
        self.type = type
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
    def __init__(self, input_str: str):
        self._input_str: str = input_str
        self._pos = self._list_pos = 0
        self.curr_char: chr = self._input_str[self._pos]
        # bool to check if the token list is populated
        self._tokl_built: bool = False
        self.token_list: list[Token] = []


    def _error(self) -> None:
        raise Exception("Error parsing the input")
    
    # build the token list from the input str
    def _build_tokens(self) -> None:
        """
        Populates the token list from the input str
        """
        while self._pos < len(self._input_str):
            # the matched pattern for any token 
            pat_match = None
            for tok_expr in TOKEN_EXPRS:
                patrn, tok_tag = tok_expr
                tok_pat = re.compile(patrn)
                pat_match = tok_pat.match(self._input_str, self._pos)
                # when the pattern is matched
                if pat_match:
                    # ? ignore whitespaces
                    if tok_tag:
                        tok_val = pat_match.group(0)
                        try:
                            # if pattern matched is an int
                            tok_val = int(pat_match.group(0))
                        except ValueError:
                            try:
                                tok_val = float(pat_match.group(0))
                            except:
                                pass
                       
                        # got the token
                        ret_token: Token = Token(tok_tag, tok_val)
                        self.token_list.append(ret_token)
                    break

            # when no pattern is matched
            if not pat_match:
                self._error()

            else:
                self._pos = pat_match.end(0)
        
        # token list is built
        self._tokl_built = True

    def _nxt_token(self) -> Token:
        """
        Return the next token from the token list
        """
        if not self._tokl_built:
            self._build_tokens()

        if self._list_pos > len(self.token_list) - 1:
            return Token(EOF, None)
        
        token: Token = self.token_list[self._list_pos]
        self._list_pos += 1
        return token


class Interpreter:
    """
    Class attributes:
    self.lexer : The Lexer instance to avoid crowding in this class
    self._curr_token : The current token parsed (Token)

    Methods:
    
    self._error : When any parsing error occurs
    """
    def __init__(self, lexer):
        self.lexer: Lexer = lexer
        # set the current token via lexer's next token method
        self._curr_token: Token = self.lexer._nxt_token()
  
    def _error(self) -> None: 
        raise SyntaxError("Invalid syntax")
    
    def consume(self, token_type: str) -> None:
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
        token: Token = self._curr_token

        if token.type == NUMBER:
            self.consume(self._curr_token.type)
            
            # take a peek at the next token...
            # if two integer tokens are side by side without any operator between them, thats a syntax error
            nxt_tok: Token = self._curr_token
            if nxt_tok.type == NUMBER:
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
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            # call factor recursively, Eg. 3^3^3
            # it evals to 3^27
            res = opr_funcs[token.val](res, self._factor())

        return res

    def _term(self):
        """
        The term as defined in the grammar of chaining (no. 2)
        """
        # result from calling the factor method
        res_fac = self._factor()

        while self._curr_token.type in (MUL, DIV):
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            res_fac = opr_funcs[token.val](res_fac, self._factor())
        
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
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            res = opr_funcs[token.val](res, self._term())

        # return the res, once all the input is parsed
        return res


def main():
    while True:
        try:
            input_str: str = input("calc> ")
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
