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

#fun(x), usage may be like y = fun(x);
def p_exp_fcall(p): 
    """exp : NAME LPAREN optargs RPAREN"""
    p[0] = ("FCall",p[1],p[3]) # optsargs may be empty (p[2])

# bullao fun(x);
# note, stmt calls is different from exp call, but both trigger same function expression
def p_stmt_fcall(p): 
    """stmt : CALL NAME LPAREN optargs RPAREN"""
    p[0] = ("FCall",p[2],p[4]) # optsargs may be empty (p[2])

def p_stmt_fret(p): 
    """stmt : RETURN exp"""
    p[0] = ("FReturn",p[2])

def p_stmt_funcdef(p): 
    """stmt : FUNCTION NAME LPAREN optargs RPAREN stmtblock"""
    p[0] = ("Func",p[2],p[4],p[6]) # optsargs may be empty (p[2])

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
    """exp : NAME DOT INDEX LPAREN exp RPAREN"""
    p[0] = ('GetIndex', p[1], p[5])

def p_stmt_listslice(p):
    """stmt : NAME DOT SLICE LPAREN exp COMMA exp RPAREN"""
    p[0] = ('ListF', 'Slice', p[1], p[5], p[7])

# do not allow creation of structs inside functions, these definitions must be global, structdef is global too
def p_stmt_structcreate(p):
    """stmt : STRUCT NAME SEP structargs SEP"""
    p[0] = ('SCreate', p[2], p[4])

def p_structargs(p):
    """structargs : structexp COMMA structargs
                  | structexp"""
    if len(p) < 3: # check
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_structexp(p):
    """structexp : NAME
                 | NAME EQUAL exp"""
    if len(p) == 2:
        p[0] = (p[1], None) # a (default value is None)
    else:   
        p[0] = (p[1], p[3]) # a = 1

def p_stmt_structobj(p):
    """stmt : MAKE NAME NAME"""
    # where first name is name of struct
    # second is name of object created
    p[0] = ('SMade',p[2],p[3])

def p_stmt_structvassign(p):
    """stmt : NAME DOT NAME EQUAL exp"""
    # first name is the struct obj, second is the obj variable, last is value expression
    p[0] = ('SVAssign',p[1],p[3],p[5]) # struct variable assignment

def p_exp_structvget(p):
    """exp : NAME DOT NAME"""
    # first name is the struct obj, second is the obj variable
    p[0] = ('SVGet',p[1],p[3]) # struct variable get


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

def p_stmt_break(p):
    """
    stmt : BREAK
    """
    p[0] = ('Break')

def p_stmt_continue(p):
    """
    stmt : CONTINUE
    """
    p[0] = ('Continue')

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

# allows any number of parameters
def p_stmt_print(p):
    """stmt : PRINT LPAREN optargs RPAREN"""
    p[0] = ('Print', p[3])

def p_error(p):
    print("Syntax error.")
    quit()
    return

parser = yacc.yacc() # start parsing, yacc object created
isBroken = False
globalenv = {}
structdef = {}
funcdef = {}

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
        if not e[1] in globalenv:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return globalenv[e[1]] 
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
        if not e[1] in globalenv:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return globalenv[e[1]]
    else:
        boolres = e[1]

    return boolres


def varCreate(p):
    global globalenv
    if type(p[1]) == tuple or type(p[1]) == list: # assignment tuple possible
        # initialization with creation
        varname = p[1][1]
        if varname in globalenv:
            raise KeyError(f"Error: A variable by that name already exists.")
        value = p[1][2]
        if type(value) == list:
            varlist = []
            for element in value:
                varlist.append(element[1]) # assuming not list inside, only single var
            globalenv[varname] = varlist
        else:
            #print("Current tree in varCreate", p, "Value",value)
            globalenv[varname] = expEvaluate(value) # where p[1][1] will be the NAME (LHS), p[1][2] the expression to be evaluated (RHS)
    else:
        # only creation, default value None assigned
        varname = p[1]
        if varname in globalenv:
            raise KeyError(f"Error: A variable by that name already exists.")
        globalenv[varname]= None

def functionCall(f):
    func = f[1]
    fargs = f[2]
    if not func in funcdef:
        raise NameError(f"Error: No such function \'{func}\' defined.")

    defined_fargs = funcdef[func][0]    
    if len(fargs) > len(defined_fargs):
        raise RuntimeError(f"Error: Function {func} was given too many arguments.")

    # more error handling for args, but assume same number of args for now
    templateFunction(fargs, funcdef[func][0])
    


def expEvaluate(e):
    global globalenv
    global structdef
    global funcdef
    #print(f"exp: {e}")    
    if e == None: # none expression
        return None

    etype = e[0]
    if etype == 'BinaryEXP':
        return eeb(e) # evaluate binary expression
    elif etype == 'CompareEXP':
        return eec(e) # evaluate expression comparison
    elif etype == 'Group':
        return expEvaluate(e[1])
    elif etype == 'Name':
        if not e[1] in globalenv:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return globalenv[e[1]]
    elif etype == 'FCall':
        return functionCall(e)
    elif etype == 'GetIndex':
        listvar = e[1]
        if listvar in globalenv and type(globalenv[listvar]) == list:
            index = eeb(e[2])
            try:
                return globalenv[listvar][index]
            except:
                raise NameError("Error: List index out of range.") ###   
        else:
            raise NameError("Error: List error.")
    elif etype == 'SVGet':
        structObj = e[1]
        varName = e[2]
        if not structObj in globalenv:
            raise NameError(f"Error: No such variable \'{structObj}\' exists.") 
        
        if not varName in globalenv[structObj]:
            raise NameError(f"Error: No such variable \'{varName}\' exists inside that struct.") 
        varVal = globalenv[structObj][varName]
        return varVal
    elif etype == 'Dec':
        return eeb(e[1]) - 1
    elif etype == 'Inc':
        return eeb(e[1]) + 1
    else: # Num, String, Char etc.
        return e[1]


def blockEvaluate(p): # p[0] == 'Block'
    global globalenv
    global isBroken
    stmtlist = p[1]
    for stmt in stmtlist:
        if stmt == 'Continue':
            break # breaking this iteration of the loop OR block later, so this is continue actually
        if stmt == 'Break':
            isBroken = True # this also breaks the next iterations/stmts, only loops turn this off after execution, so give error in normal blocks
            break # break this iteration
        if stmt != None and stmt[1] != []:
            stmtEvaluate(stmt)
            

def runProgram(p): # p[0] == 'Program': a bunch of statements
    global globalenv
    proglist = p[1]  
    for stmt in proglist: # statements in proglist
        if stmt != None and stmt[1] != []:
            stmtEvaluate(stmt[1]) # since stmt[0] == 'Stmt' here in Program

def stmtEvaluate(p): # p is the parsed tree / program, evalStmts
    global structdef, funcdef, globalenv, isBroken
    #print(f"stmt: {p}")    
    stype = p[0] # node type of parse tree
    if stype == 'DecR':
        globalenv[p[1][1]] -= 1
    elif stype == 'IncR':
        globalenv[p[1][1]] += 1
    elif stype == '=': # assignment
        if not p[1] in globalenv:
            raise NameError(f"Error: No such variable \'{p[1]}\' exists.")
        globalenv[p[1]] = expEvaluate(p[2]) # make an entry for the variable in the globalenv dictionary, where the key is the NAME and the value is the assigned expression
    elif stype == 'Made': # creation of a variable
        varCreate(p)
    elif stype == 'If':    
        condition = p[1]
        then_stmts = p[2]
        #print(f"Condition: {condition} Then {then_stmts}")
        if eec(condition):
            #print("Running then statements in if")
            blockEvaluate(then_stmts)
    elif stype == 'IfElse':    
        condition = p[1]
        then_stmts = p[2]
        else_stmts = p[3]
        if eec(condition):
            blockEvaluate(then_stmts)
        else:
            blockEvaluate(else_stmts)
    elif stype == 'IfElifElse':
        condition = p[1]
        then_stmts = p[2]
        elif_stmts = p[3]
        else_stmts = p[4]
        if eec(condition):
            blockEvaluate(then_stmts)
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
                blockEvaluate(es_then)
            else:
                blockEvaluate(else_stmts)
    elif stype == 'For':
        countervar = p[1]
        if not countervar in globalenv:
            raise NameError(f"Error: No such variable \'{countervar}\' exists.") 
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
        
        old_counterval = globalenv[countervar]
        for i in range(start, end+1, step):
            globalenv[countervar] = i
            blockEvaluate(do_stmts)
            if isBroken:
                break
        isBroken = False # reset break
        globalenv[countervar] = old_counterval

    elif stype == 'DoWhile':
        do_stmts = p[1]
        condition = p[2]
        blockEvaluate(do_stmts) # first time the block is just executed
        while eec(condition): # condition is necessary then onwards
            if isBroken:
                break
            blockEvaluate(do_stmts)
        isBroken = False
    elif stype == 'Print':
        printList = p[1]
        for element in printList:
            result = expEvaluate(element)
            if type(result) == bool:
                if result == True:
                    result = 'sach'
                elif result == False:
                    result = 'ghalat'
            print(result, end=" ")
        print()  
    elif stype == 'FCall':
        expEvaluate(p)
        # nothing is returned in the simple void stmt function call
    elif stype == 'FReturn':
        returnVal = expEvaluate(p[1])
        return returnVal
    elif stype == 'Func':
        func = p[1]
        fargs = p[2]
        fstmts = p[3]
        if func in funcdef:
            raise KeyError(f"Function {func} has already been defined.")
        funcdef[func] = [fargs, fstmts] # default values of args, fargs must be resolved further

    elif stype == 'ListF':
        listvar = p[2]
        if listvar in globalenv and type(globalenv[listvar]) == list:
            ftype = p[1]
            e1 = eeb(p[3]) # evaluate expression, error handling
            vlist = globalenv[listvar]
            if ftype == 'Pop':
                if e1 == 0: # remove head of list
                    del globalenv[listvar][0]
                elif e1 == 1: # remove tail of list
                    globalenv[listvar].pop()
            elif ftype == 'Push': #append
                globalenv[listvar].append(e1)
            elif ftype == 'Slice':
                e2 = eeb(p[4])
                globalenv[listvar] = vlist[e1:e2+1]
        else:
            raise NameError("List error.")
    elif stype == 'SCreate':
        structname = p[1]
        structargs = p[2]
        if structname in structdef:
            raise KeyError(f"Error: A struct by that name is already defined.")
        structdef[structname] = {} # create empty definition for given struct initially
        # add structargs / member variables
        for sarg in structargs:
            varName = sarg[0]
            varVal = sarg[1]
            if varName in structdef[structname]:
                raise KeyError(f"Error: Duplicate variable in stuct definition.") 
            structdef[structname][varName] = expEvaluate(varVal) # varVal may be None here.
    
    elif stype == 'SMade':
        structname = p[1]
        varName = p[2] # object variable
        if not structname in structdef:
            raise NameError(f"Error: No struct of that name is defined.")
        
        if varName in globalenv:
            raise KeyError(f"Error: This variable already exists.") 
        
        globalenv[varName] = structdef[structname]
    
    elif stype == 'SVAssign':
        structObj = p[1]
        varName = p[2]
        varVal = p[3]
        if not structObj in globalenv:
            raise NameError(f"Error: No such variable \'{structObj}\' exists.") 
        
        if not varName in globalenv[structObj]:
            raise NameError(f"Error: No such variable \'{varName}\' exists inside that struct.") 
        
        globalenv[structObj][varName] = expEvaluate(varVal)
    else:       
        if p[1] in globalenv:
            return globalenv[p[1]]
        else:    
            return p[1]
    
    #print("GLOBAL ENV:",globalenv)

def templateFunction(fargs, fstmtsblock):
    print("Inside function")
    print(fargs)
    stmtlist = fstmtsblock
    for stmt in stmtlist:
        if stmt != None and stmt[1] != []:
            stype = stmt[0]
            if stype == 'FReturn':
                returnVal = expEvaluate(p[1])
                return returnVal
                continue # skip evaluation of ret/call stmt
            elif stype == 'FCall':
                return functionCall(stmt)
                continue
            stmtEvaluate(stmt)
            print(globalenv)
    
    
while True:
    break
    userin = input("{YAPL_UZ} ")
    uzparsed = parser.parse(userin)
    if not uzparsed:
        continue
    try:   
        result = stmtEvaluate(('Program',uzparsed))
    except Exception as e:
        print(e)

while True:
    globalenv.clear() #refresh state
    structdef.clear()
    funcdef.clear()
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
        runProgram(('Program',uzparsed))
    except Exception as e:
        print(e)
    
    input("Press any key to run code again.")


exit()