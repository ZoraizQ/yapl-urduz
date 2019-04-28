import sys
import ply.lex as lex
import re

#TOKEN IDENTIFIERS, have to match name of our token
#Reserved keywords for YAPL_UZ
reserved = { # single rule, special
    'agar' : 'IF',
    'warna' : 'ELSE',
    'phir':'THEN',
    'warnaagar' : 'ELIF',
    'challo':'FOR',
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
    'MODULUS',
    'POWER',
    'EQUAL',
    'NAME',
    'COMMENT',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'LBRACK',
    'RBRACK',
    'SEP',
    'SEMICOL',
    'COMMA',
    'LT',
    'GT',
    'LTE',
    'GTE',
    'NE',
    'EE',
    'STRING',
    'INC',
    'DEC'
] + list(reserved.values())

t_PLUS = r'\+' # recognise regular expression symbol
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_POWER = r'\^'
t_MODULUS = r'\%'
t_EQUAL = r'\='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_SEP = r'\|'
t_LT = r'\<'
t_GT = r'\>'
t_LTE = r'\<\='
t_GTE = r'\>\='
t_NE = r'\!\='
t_EE = r'\=\='
t_INC = r'\+\+'
t_DEC = r'\-\-'
t_COMMA = r'\,'
t_SEMICOL = r'\;'
t_ignore = r' \t\v\r' # ignore spaces, better lexing performance, special case
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

# Handle EOF, comment
'''
def t_eof(t):
more = raw_input('... ')
if more:
 self.lexer.input(more)
 return self.lexer.token()
return None
'''
'''
https://dev.to/geocine/what-are-your-favorite-programming-language-syntax-features-1emm
optional chaining operator
'''
def t_STRING(t):
    r'\"[^"]*\"'
    t.value = t.value[1:-1] # truncate "" marks
    return t

# variable or function names
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')
    return t

def t_FLOAT(t):
    r'\d*\.\d+f?' # any number of digits >= 1 before and after the decimal, and optional f
    if t.value[-1] == 'f':
        t.value = t.value[:-1]
    t.value = float(t.value)
    return t

def t_INT(t): # parameter t is the token
    r'\d+' # atleast one digit
    t.value = int(t.value) # convert to integer
    return t

def t_error(t): # error while lexing
    print(f"Lexer Error 0: Illegal character entered = {t.value[0]}.")
    t.lexer.skip(1) # skips illegal character


# lexer build
uzlexer = lex.lex()

'''
while True:
    try:
        print("YAPL_UZ>>",end='')
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
