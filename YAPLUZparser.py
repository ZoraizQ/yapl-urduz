import ply.yacc as yacc
import YAPLUZlexer
import math
import random
import sys

#sys.tracebacklimit = 0 # to prevent traceback debug output since it is not needed

tokens = YAPLUZlexer.tokens
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

#to resolve ambiguity, individual tokens assigned a precedence level and associativity. 
# tokens ordered from lowest to highest precedence, rightmost terminal judged
precedence = (
    ('left', 'DEC','INC'),
    ('left','OR'),
    ('left','AND'),
    ('left','EE','NE'),
    # to not allow associative combinations like a < b < c -> 'nonassoc' used
    ('nonassoc', 'LT', 'GT', 'GTE', 'LTE'),
    ('left', 'PLUS', 'MINUS'), # +, - same precedence, left associative
    ('left', 'MULTIPLY', 'DIVIDE'), # *, / same precedence but more than +,- and left associative
    ('right', 'NOT') #unary
)


start = 'uz'
# multiple variables, assigning data from one variable to another

# after the lexing, start parsing
def p_uz(p): # non-terminal, starting
    """
    uz : line uz
    """
    p[0] = [p[1]] + p[2] # list comprehension used to solve recursive grammar, added/appending as well
    
def p_line_stmt(p): # non-terminal, starting
    """
    line : stmt SEMICOL
    """
    p[0] = ('Stmt',p[1])
    #print(p.lineno(2))
    '''
    try:
        run(p[0])
    except Exception as e:
        print(e)
    '''
def p_uz_stmt_error(p):
    """uz : error SEMICOL
    """
    print("Syntax error in uz statement. Bad statement")
    
def p_uz_empty(p):
    """
    uz :
    """
    p[0] = []
    
def p_stmt_block(p): 
    """ stmtblock : LBRACE stmtS RBRACE"""
    p[0] = ('Block', p[2])
    ''' stmtblock : LBRACE new_scope stmtS RBRACE
    pop_scope()        # Return to previous scope'''
'''
def p_new_scope(p):
    "new_scope :"
    # Create a new scope for local variables
    s = new_scope()
    push_scope(s)
    ...
'''

def p_stmt_block_error(p):
    """ stmtblock : LBRACE error RBRACE"""
    p[0] = "BAD STMTS IN BLOCK"

def p_stmtS(p):
    """
    stmtS : stmt SEMICOL stmtS
    """
    p[0] = [p[1]]+p[3]

def p_stmtS_error(p):
    """
    stmtS : error SEMICOL stmtS
    """
    p[0] = "BAD STMT IN STMTS"

def p_stmtS_empty(p):
    """
    stmtS :
    """
    p[0] = []

# need to create tuples for p to represent parse tree, and think of parse tree format that is perfect
# ('*', 3,('+',9,1)) == 3*9+1 == 28
# p[2] is the binary operator here, p[1] is the left expression, p[2] is the right expression
#p.lineno(num). Return the line number for symbol num
#p.lexpos(num). Return the lexing position for symbol num 

def p_exp_call(p):
    """exp : CALL NAME LPAREN optargs RPAREN"""
    p[0] = ("Call",p[2],p[4]) # optsargs may be empty (p[2])

def p_optargs(p):
    """optargs : args"""
    p[0] = p[1]

def p_optargs_empty(p): # can go to empty as well
    """optargs :"""
    p[0] = []

def p_args(p):
    """args : exp COMMA args
            | exp"""
    if len(p) <= 3: # check
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_exp_bin(p):
    """ 
    exp : exp PLUS exp
        | exp MINUS exp
        | exp MULTIPLY exp
        | exp DIVIDE exp
        | exp POWER exp
        | exp MODULUS exp
    """
    p[0] = ('BinaryEXP', p[2], p[1], p[3])

def p_exp_compare(p):
    """
    exp : exp AND exp
        | exp OR exp
        | exp GT exp
        | exp LT exp
        | exp GTE exp
        | exp LTE exp
        | exp EE exp
        | exp NE exp
    """
    p[0] = ('CompareEXP', p[2], p[1], p[3])
    
# needs work
def p_exp_name(p):
    """exp : NAME"""
    p[0] = ('Name', p[1])

def p_list(p):
    """list : LBRACK optargs RBRACK"""
    p[0] = p[2]

def p_stmt_listpop(p):
    """stmt : NAME DOT POP LPAREN exp RPAREN"""
    p[0] = ('ListF', 'Pop', p[1], p[5])

def p_stmt_listpush(p):
    """stmt : NAME DOT PUSH LPAREN exp RPAREN"""
    p[0] = ('ListF', 'Push', p[1], p[5])

def p_stmt_listindex(p):
    """stmt : NAME DOT INDEX LPAREN exp RPAREN"""
    p[0] = ('ListF', 'Index', p[1], p[5])

def p_stmt_listslice(p):
    """stmt : NAME DOT SLICE LPAREN exp COMMA exp RPAREN"""
    p[0] = ('ListF', 'Slice', p[1], p[5], p[7])

def p_exp_num(p): # handles unary minus, minusminus, plusplus
    """
    exp : MINUS INT
        | MINUS FLOAT
        | INT
        | FLOAT
    """
    if len(p) == 3:
        p[0] = ('Num', -p[2])
    else:
        p[0] = ('Num', p[1])    

def p_exp_leftincdec(p):
    """
    exp : DEC exp
        | INC exp
    """
    if p[1] == '--':
        p[0] = ('Dec', p[2])
    elif p[1] == '++':
        p[0] = ('Inc', p[2])

def p_stmt_rightincdec(p):
    """
    stmt : exp DEC
         | exp INC
    """
    if p[2] == '--':
        p[0] = ('DecR', p[1])
    elif p[2] == '++':
        p[0] = ('IncR', p[1 ])
    
def p_exp_not(p):
    """exp : NOT exp"""
    p[0] = ('Not', p[2])

def p_exp_boolean(p):
    """
    exp : TRUE
        | FALSE
    """
    if p[1] == 'sach':
        p[0] = ('Bool', True)
    else:
        p[0] = ('Bool', False)

def p_exp_string(p):
    """exp : STRING"""
    p[0] = ('Str', p[1])

def p_exp_char(p):
    """exp : CHAR"""
    p[0] = ('Char', p[1])

def p_exp_group(p):
    """exp : LPAREN exp RPAREN"""
    p[0] = ('Group', p[2])

# banao a, or banao a = 3 (initialization and creation of variables)
def p_makevar(p):
    """stmt : MAKE NAME"""
    p[0] = ('Made', p[2]) 

def p_initvar(p):
    """stmt : MAKE NAME EQUAL exp
            | MAKE NAME EQUAL list"""
    p[0] = ('Made', ('=',p[2],p[4])) 

def p_assignment(p):
    """stmt : NAME EQUAL exp"""
    p[0] = ('=',p[1],p[3])

def p_if(p):
    """
    stmt : IF exp THEN stmtblock
         | IF exp THEN stmtblock elsif ELSE THEN stmtblock
    """
    len_p = len(p)
    if len_p == 5:
        p[0] = ('If', p[2],p[4])
    else:
        if p[5] == []:
            p[0] = ('IfElse', p[2],p[4],p[8])
        else:
            p[0] = ('IfElifElse', p[2],p[4],p[5],p[len_p-1])

def p_elsif(p):
    """
    elsif : ELIF exp THEN stmtblock elsif
    """
    p[0] = [(p[2],p[4])] + p[5]
    
def p_elsif_empty(p):
    """
    elsif :
    """
    p[0] = []


# karo {dekhao(x); x++;} jabtak (x != 4);
def p_stmt_while(p):
    """
    stmt : DO stmtblock WHILE exp
    """
    p[0] = ('DoWhile', p[2], p[4])

'''
banao i;
challo i = 5 se 1 tak -2
{
dekhao(i);
};
'''
def p_stmt_for(p):
    """stmt : FOR NAME EQUAL exp TO exp UNTIL step stmtblock"""
    p[0] = ('For', p[2], p[4], p[6], p[9], p[8])

def p_step(p):
    """step : exp"""
    p[0] = p[1]

def p_step_empty(p):
    """step : """
    p[0] = []

# allow more parameters
def p_stmt_print(p):
    """stmt : PRINT LPAREN exp RPAREN"""
    p[0] = ('Print', p[3])
   
def p_stmt_return(p):
    """stmt : RETURN exp"""
    p[0] = ('Return', p[2])

def p_error(p):
    print("Syntax error.")
    quit()
    return

parser = yacc.yacc() # start parsing, yacc object created
var_dict = {}

def eeb(e): #evaluate expression binary
    operator = e[1]
    if e[0] == 'Group':
        return eeb(e[1])
    elif e[0] == 'Dec':
        return eeb(e[1]) - 1
    elif e[0] == 'Inc':
        return eeb(e[1]) + 1
    elif operator == '+':
        return eeb(e[2]) + eeb(e[3])
    elif operator == '-':
        return eeb(e[2]) - eeb(e[3])
    elif operator == '*':
        return eeb(e[2]) * eeb(e[3])
    elif operator == '/':
        denom = eeb(e[3])
        if denom == 0:
            raise ZeroDivisionError("Error: Division by 0.")
        return float(eeb(e[2])) / denom
    elif operator == '^':
        return abs(eeb(e[2]))** eeb(e[3])
    elif operator == '%':
        return eeb(e[2]) % eeb(e[3])
    elif e[0] == 'Name':
        if not e[1] in var_dict:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return var_dict[e[1]] 
    else:
        if type(e[1])==str or type(e[1])==bool:
            raise TypeError("Error: Type not supported with this operation.")
        return e[1]

    
def eec(e): # evaluate expression comparison
    #print(f"CT in eec: {e}")
    operator = e[1]
    if e[0] == 'Group':
        return eec(e[1])
    elif e[0] == 'BinaryEXP':
        return eeb(e)
    elif operator == '<':
        boolres = eec(e[2]) < eec(e[3])
    elif operator == '<=':
        boolres = eec(e[2]) <= eec(e[3])
    elif operator == '>':
        boolres = eec(e[2]) > eec(e[3])
    elif operator == '>=':
        boolres = eec(e[2]) >= eec(e[3])
    elif operator == '!=':
        boolres = eec(e[2]) != eec(e[3])
    elif operator == '==':
        boolres = eec(e[2]) == eec(e[3])
    elif operator == 'aur':
        paramL = eec(e[2])
        paramR = eec(e[3])
        if type(paramL) == str or type(paramL) == int or type(paramL) == float:
            raise TypeError("Error: Type not supported with this operation.")
        elif type(paramR) == str or type(paramR) == int or type(paramR) == float:
            raise TypeError("Error: Type not supported with this operation.")
        boolres = paramL and paramR
    elif operator == 'ya':
        paramL = eec(e[2])
        paramR = eec(e[3])
        if type(paramL) == str or type(paramL) == int or type(paramL) == float:
            raise TypeError("Error: Type not supported with this operation.")
        elif type(paramR) == str or type(paramR) == int or type(paramR) == float:
            raise TypeError("Error: Type not supported with this operation.")
        boolres = paramL or paramR
    elif e[0] == 'Name':
        if not e[1] in var_dict:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return var_dict[e[1]]
    else:
        boolres = e[1]

    return boolres

def run(p): # p is the parsed tree / program
    #print(f"CT: {p}")
    if p[0] == 'Program':
        proglist = p[1]  
        for i in range(len(proglist)):
            if proglist[i] != None:
                run(proglist[i])

    stype = p[0] # node type of parse tree
    if stype == 'Stmt':
        if p[1] != None and p[1] != []:
            return run(p[1])
    elif stype == 'Block': # run all statements in block of statement
        slist = p[1]
        for i in range(len(slist)):
            if slist[i] != None:
                run(slist[i])
    elif stype == 'Name':
        if not p[1] in var_dict:
            raise NameError(f"Error: No such variable \'{p[1]}\' exists.")
        return var_dict[p[1]]
    elif stype == 'Group':
        return run(p[1])
    elif stype == 'BinaryEXP':
        return eeb(p) # evaluate expression binary
    elif stype == 'DecR':
        var_dict[p[1][1]] -= 1
    elif stype == 'IncR':
        var_dict[p[1][1]] += 1
    elif stype == 'Dec':
        return run(p[1]) - 1
    elif stype == 'Inc':
        return run(p[1]) + 1
    elif stype == 'CompareEXP':
        return eec(p) # evaluate expression comparison
    elif stype == '=': # assignment
        if not p[1] in var_dict:
            raise NameError(f"Error: No such variable \'{p[1]}\' exists.")
        var_dict[p[1]] = run(p[2]) # make an entry for the variable in the var_dict dictionary, where the key is the NAME and the value is the assigned expression
    elif stype == 'Made': # creation of a variable
        if type(p[1]) == tuple or type(p[1]) == list: # assignment tuple possible
            # initialization with creation
            varname = p[1][1]
            if varname in var_dict:
                raise KeyError(f"Error: A variable by that name already exists.")
            value = p[1][2]
            if type(value) == list:
                varlist = []
                for element in value:
                    varlist.append(element[1]) # assuming not list inside, only single var
                var_dict[varname] = varlist
            else:    
                var_dict[varname] = run(value) # where p[1][1] will be the NAME (LHS), p[1][2] the expression to be evaluated (RHS)
        else:
            # only creation, default value None assigned
            varname = p[1]
            if varname in var_dict:
                raise KeyError(f"Error: A variable by that name already exists.")
            var_dict[varname]= None
    elif stype == 'If':    
        condition = p[1]
        then_stmts = p[2]
        #print(f"Condition: {condition} Then {then_stmts}")
        if eec(condition):
            #print("Running then statements in if")
            run(then_stmts)
    elif stype == 'IfElse':    
        condition = p[1]
        then_stmts = p[2]
        else_stmts = p[3]
        if eec(condition):
            run(then_stmts)
        else:
            run(else_stmts)
    elif stype == 'IfElifElse':
        condition = p[1]
        then_stmts = p[2]
        elif_stmts = p[3]
        else_stmts = p[4]
        if eec(condition):
            run(then_stmts)
        else:
            esTrue = False
            es_then = None
            for es in elif_stmts:
                es_condition = es[0]
                es_then = es[1]
                if eec(es_condition):
                    esTrue = True
                    break
            if esTrue and es_then != None:
                run(es_then)
            else:
                run(else_stmts)
    elif stype == 'For':
        countervar = p[1]
        if not countervar in var_dict:
            raise NameError(f"Error: No such variable \'{p[1]}\' exists.") 
        start = eeb(p[2]) # must be numerical expression
        end = eeb(p[3]) # evaluating numerical expressions
        do_stmts = p[4]
        step = p[5]
        if start > end:
            end -= 2
        if step == []:
            if start > end:
                step = -1
            else:
                step = 1
        else:
            step = p[5][1]
        if start > end and step > 0:
            raise RuntimeError("Error: Wrong step given.")
        
        old_counterval = var_dict[countervar]
        for i in range(start, end+1, step):
            var_dict[countervar] = i
            run(do_stmts)

        var_dict[countervar] = old_counterval

    elif stype == 'DoWhile':
        do_stmts = p[1]
        condition = p[2]
        run(do_stmts) # first time the block is just executed
        while eec(condition): # condition is necessary then onwards
            run(do_stmts)
    elif stype == 'Print':
        result = run(p[1])
        if type(result) == bool:
            if result == True:
                result = 'sach'
            elif result == False:
                result = 'ghalat'
        print(result)  
    elif stype == 'ListF':
        listvar = p[2]
        if listvar in var_dict and type(var_dict[listvar]) == list:
            ftype = p[1]
            e1 = eeb(p[3]) # evaluate expression, error handling
            vlist = var_dict[listvar]
            if ftype == 'Pop':
                if e1 == 0: # remove head of list
                    del var_dict[listvar][0]
                elif e1 == 1: # remove tail of list
                    var_dict[listvar].pop()
            elif ftype == 'Push': #append
                var_dict[listvar].append(e1)
            elif ftype == 'Index':
                print(vlist[e1])
            elif ftype == 'Slice':
                e2 = eeb(p[4])
                var_dict[listvar] = vlist[e1:e2+1]

    else:       
        if p[0] == 'Program':
            return p[1]
        elif p[1] in var_dict:
            return var_dict[p[1]]
        else:    
            return p[1]
    
    #print("Variables:",var_dict)


while True:
    break
    userin = input("{YAPL_UZ} ")
    uzparsed = parser.parse(userin)
    if not uzparsed:
        continue
    try:   
        result = run(('Program',uzparsed))
    except Exception as e:
        print(e)

while True:
    #refresh state
    var_dict.clear()
    fileHandler = open(sys.argv[1],"r")
    userin = fileHandler.read()
    fileHandler.close()
    #print(userinlist)

    #print('\x1bc',end="")
    print("{YAPL_UZ}")
    uzparsed = parser.parse(userin)
    if not uzparsed:
        continue
    
    print(userin)
    print("=========================================\n{OUTPUT}")
    try:
        run(('Program',uzparsed))
    except Exception as e:
        print(e)
    
    input("Press any key to run code again.")


exit()