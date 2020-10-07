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
# - [x] Build an AST (abstract syntax tree)
# - [x] Interpret using the AST


# Grammar
# 1) Chaining + and - operators (eg. 4 - 5 + 3 - 7) 
################################################################
#   expr : term ((ADD | SUB) term)*                            #                       
#   term : NUMBER                                              #                              
#                                                              # 
################################################################

# 2) Chaining all the operators together (eg. 7 - 5 * 2 ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            # 
#   term : factor ((MUL | DIV) factor)*                        # 
#   factor : base (RAISETO factor)*                            # 
#   base : NUMBER                                              # 
#                                                              #     
################################################################

# 3) Chaining with parenthesis (eg. 7 - (5 * 2) ^ 3)
################################################################
#   expr : term ((ADD | SUB) term)*                            #     
#   term : factor ((MUL | DIV ) factor)*                       # 
#   factor : base (RAISETO factor)*                            #             
#   base: NUMBER | \( expr \)                                  #             
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


# ====================== PARSER (using AST) ======================= #
class AST:
    """
    The base node class
    """
    pass

class BinOprNode(AST):
    """
    The node class to represent the binary operators

    left_child : holds the operand on the left (can be another BinOprNode) 
    right_child : holds the operand on the right (can be another BinOprNode)

    Eg.
            "*"
           /   \
         "-"   "7"
        /   \
       "2"  "3"

    This will result in the expression: (2 - 3) * 7

    """
    def __init__(self, left_child, opr, right_child):
        self.left_child = left_child
        self.token = self.opr = opr
        self.right_child = right_child


class NumNode(AST):
    """
    The node class to represent numbers
    """
    def __init__(self, num_tok: Token):
        self.num_tok: Token = num_tok
        self.node_val = num_tok.val
    

class Parser:
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
            # if two integer tokens are side by side 
            # without any operator between them, thats a syntax error
            nxt_tok: Token = self._curr_token
            if nxt_tok.type == NUMBER:
                self._error()
            
            return NumNode(token)
        
        elif token.type == LPAREN:
            self.consume(LPAREN)
            # call expr for calculating the expression inside both the parenthesis
            node = self.expr()
            # after the expression is done parsing, the right parenthesis will only be present
            # as no other method is checking for the right paren token type
            self.consume(RPAREN)
            # return the node
            return node
        
    def _factor(self):
        """
        The factor as defined in the grammar of chaining (no. 2)
        """
        # node from calling the base method
        node_base = self._base()

        while self._curr_token.type == EXP:
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            # call factor recursively, Eg. 3^3^3
            # equals to 3^27
            node_base = BinOprNode(left_child=node_base, opr=token, right_child=self._factor())

        return node_base

    def _term(self):
        """
        The term as defined in the grammar of chaining (no. 2)
        """
        # node from calling the factor method
        node_fac = self._factor()

        while self._curr_token.type in (MUL, DIV):
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            
            node_fac = BinOprNode(left_child=node_fac, opr=token, right_child=self._factor())
        
        return node_fac

    # build the root node
    def expr(self):
        """
        Builds the AST root node
        """
        # finally get the term
        node_term = self._term()

        while self._curr_token.type in (ADD, SUB):
            token: Token = self._curr_token
            self.consume(self._curr_token.type)
            
            node_term = BinOprNode(left_child=node_term, opr=token, right_child=self._term())

        # return the root node
        return node_term

    def parse(self):
        """
        Returns the root node of the AST i.e. self.expr()
        """
        return self.expr()


# =========================== INTERPRETER (using NodeVisitor) =================== #
class NodeVisitor:
    """
    The nodevisitor that dispatches the appropriate method for the 
    type of Node visited
    """
    def _visit(self, node):
        method_name = "visit_" + type(node).__name__

        # the dispatched method for the node i.e. visit_BinOprNode or visit_NumNode
        # when the attr is not present, visit_func is then the _generic_visit method
        visit_func = getattr(self, method_name, self._generic_visit)
        return visit_func(node)

    def _generic_visit(self, node):
        raise NotImplementedError(f"function visit_{type(node).__name__} is not implemented")


class Interpreter(NodeVisitor):
    """
    The interpreter class to interpret the prog
    """
    def __init__(self, parser: Parser):
        # takes in a parser 
        self.parser = parser

    def visit_BinOprNode(self, curr_node):
        """
        Return the calculated result from the type of operator by visiting the nodes
        """
        # essentially it takes the node's token val, which in case of an operator
        # is its string representation. Then it looks up the opr_funcs dict 
        # for the operator function related to the string repr and calculates the result,
        # on the left and right childs of the curr_node (which inturn can be expressions)
        # ? i agree this is plain confusing to read
        return opr_funcs[curr_node.token.val](self._visit(curr_node.left_child),
                self._visit(curr_node.right_child))
        
    # when the dispatched method is visit_NumNode, return the node's value
    def visit_NumNode(self, curr_node):
        return curr_node.node_val

    # finally...
    def interpret(self):
        root_node = self.parser.parse()
        # ? the visiting starts
        return self._visit(root_node)


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
        parser = Parser(lexer)
        interp = Interpreter(parser)
        res = interp.interpret()
        print(res)

if __name__ == "__main__":
    main()
