import sys
import ply.lex as lex
import ply.yacc as yacc
import re

# parameters like input 
'''
banao myNum = 0
@ strong comment?
~ single-line comment = print numbers 1 to 10
~~~
multiline comment
~~~
karo {
myNum += 1
dekhao(myNum)
} jabtak (myNum != 100) aur jabtak (myNum !=5)
'''

#TOKEN IDENTIFIERS
# has to match name of our token,   
#YAPL_URDUZ
reserved = { # single rule, special
    'agar' : 'IF',
    'warna' : 'THEN',
    'warnaagar' : 'ELIF',
    'bhejo' : 'RETURN',
    'dekhao' : 'PRINT',
    'jabtak' : 'WHILE',
    'karo' : 'DO',
    'se' : 'TO', # for loop in range
    'tak' : 'UNTIL',
    'banao' : 'MAKE',
    'mayhar':'EACH', # for each loop
    'kayliye':'IN',
    'kaam':'FUNCTION',
    'aur':'AND', # can be caps too
    'ya':'OR',
    'na':'NOT',
    'bullao':'CALL',
    'sach':'TRUE',
    'ghalat':'FALSE'
}

# list of all possible tokens, the name 'tokens' is necessary
tokens = [
    'INT',
    'FLOAT',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
    'EQUALSTO',
    'NAME',
    'COMMENT',
    'LPAREN',
    'RPAREN'
] + list(reserved.values())

t_PLUS = r'\+' # recognise regular expression symbol
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_EQUALSTO = r'\=\='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ignore = r' ' # ignore spaces, better lexing performance, special case
# allow \' character here for urdu?
# any number of characters, upper or lower case any position, atleast 1 length
#first character must be alphabet or _, rest can be alphanumeric or have _, adding no character means concatenation

# begin from ~
def t_COMMENT(t):
 r'\~.*' # . represents any character
 pass
 # discarding tokens, since nothing is returned

# Define a rule so we can track line numbers
def t_newline(t):
 r'\n+'
 t.lexer.lineno += len(t.value) # number of \n characters lexed (length) in a line increments the lineno attribute of the t.lexer
# positional info recorded in lexpos attribute, we can get column info from this

# Handle EOF
 def t_eof(t):
     more = raw_input('... ')
     if more:
         self.lexer.input(more)
         return self.lexer.token()
     return None

'''
https://dev.to/geocine/what-are-your-favorite-programming-language-syntax-features-1emm
optional chaining operator
'''

# variable names
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')
    return t

def t_INT(t): # parameter t is the token
    r'\d+' # atleast one digit
    t.value = int(t.value) # convert to integer
    return t

def t_FLOAT(t):
    r'\d+\.\d+' # any number of digits >= 1 before and after the decimal
    t.value = float(t.value)
    return t

def t_error(t): # error while lexing
    print("Lexer Error 0: Illegal characters entered.")
    t.lexer.skip(1) # skips illegal character


# lexer build
uzlexer = lex.lex()

# after the lexing, start parsing
def p_calc(p): # non-terminal
    """ 
    calc : exp
         | E 
    """
    print(p[1])

# need to create tuples for p to represent parse tree, and think of parse tree format that is perfect
# ('*', 3,('+',9,1)) == 3*9+1 == 28
# p[2] is the binary operator here, p[1] is the left expression, p[2] is the right expression
def p_exp(p):
    """ 
    exp : exp PLUS exp
        | exp MINUS exp
        | exp MULTIPLY exp
        | exp DIVIDE exp
    """
    p[0] = (p[2], p[1], p[3])

def p_exp_int_float(p): 
    """
    exp : INT
        | FLOAT
    """
    p[0] = p[1]

def p_name_num(p):
    """
    name : NAME
    """
    p[0] = p[1]

# empty
def p_e(p): 
    """ e : """
    p[0] = None

def p_error(p):
    print("Parser Error 1: Syntax error.")
    return

parser = yacc.yacc() # start parsing, yacc object created
'''
while True:
    try:
        print("YAPL_URDUZ>>",end='')
        uzlexer.input(input()) # reset lexer, store new input
            
        while True: # necessary to lex all tokens
            tokenEntered = uzlexer.token() # return next token from lexer
            if not tokenEntered: # lexer error also given
                break
            print(tokenEntered)
    except:
        print("Unexpected error.")
        break
'''

while True:
    try:
        print("YAPL_URDUZ>> ",end='')
        userin = input()
        
    except:
        print("Unexpected error.")
        break
    parser.parse(userin)


exit()