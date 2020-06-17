import ply.yacc as yacc
import YAPLUZlexer
import math
import random
import sys
from termcolor import colored

#sys.tracebacklimit = 0 # to prevent traceback debug output since it is not needed

tokens = YAPLUZlexer.tokens
reserved = YAPLUZlexer.reserved

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

def p_line_stmt_errror(p): # non-terminal, starting
    """
    line : error SEMICOL
    """
    print ("Ghalat expression ka istamaal.")
    quit()

def p_line_semicol_errror(p): # non-terminal, starting
    """
    line : stmt error
    """
    print ("; reh gaya hai.")
    quit()
    
def p_uz_empty(p):
    """
    uz :
    """
    p[0] = []
    
def p_stmt_block(p): 
    """ stmtblock : LBRACE stmtS RBRACE"""
    p[0] = ('Block', p[2])

def p_stmt_block_lbrack_error(p): 
    """ stmtblock : error stmtS RBRACE"""
    print("{ bracket missing hai.")

def p_stmt_block_rbrack_error(p): 
    """ stmtblock : LBRACE stmtS error"""
    print("} bracket missing hai.")

def p_stmt_block_stmtS_error(p): 
    """ stmtblock : LBRACE error RBRACE"""
    print("StmtS ka error iss block mai.")

def p_stmtS(p):
    """
    stmtS : stmt SEMICOL stmtS
    """
    p[0] = [p[1]]+p[3]

def p_stmtS_error(p):
    """
    stmtS : stmt error stmtS
    """
    print ("; reh gaya hai.")
    quit()

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
    """optargs : """
    p[0] = []

def p_args(p):
    """args : exp COMMA args
            | exp"""
    if len(p) <= 3: # check
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_exp_not(p):
    """exp : NOT exp"""
    p[0] = ('Not', p[2])

def p_exp_leftincdec(p):
    """
    exp : DEC exp
        | INC exp
    """
    if p[1] == '--':
        p[0] = ('Dec', p[2])
    elif p[1] == '++':
        p[0] = ('Inc', p[2])

def p_exp_bin(p):
    """ 
    exp : exp PLUS exp
        | exp MINUS exp
        | exp MULTIPLY exp
        | exp DIVIDE exp
        | exp POWER exp
        | exp MODULUS exp
    """
    L = p[1][0]
    R = p[3][0]
    blacklist = ['Char','Str','Bool','CompareEXP','Not']
    if L in blacklist or R in blacklist:
        print(f"Incorrect binary expression operands in line {p.lineno(0)+1}.")
        quit()
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
    p[0] = ("FCall",p[1],p[3]) # optsargs may be empty (p[3])

# bullao fun(x);
# note, stmt calls is different from exp call, but both trigger same function expression
def p_stmt_fcall(p): 
    """stmt : CALL NAME LPAREN optargs RPAREN"""
    p[0] = ("FCall",p[2],p[4]) # optsargs may be empty (p[4])

def p_stmt_fret(p): 
    """stmt : RETURN exp"""
    p[0] = ("FReturn",p[2])

def p_stmt_funcdef(p): 
    """stmt : FUNCTION NAME LPAREN funcargs RPAREN stmtblock"""
    p[0] = ("Func",p[2],p[4],p[6]) # structargs used because some parameters can have None value, and some pre-defined

def p_funcargs(p):
    """funcargs : funcexp COMMA funcargs
                | funcexp"""
    if len(p) < 3: # check
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_funcargs_empty(p):
    """funcargs : """
    p[0] = []

def p_funcexp(p):
    """funcexp : NAME
               | NAME EQUAL exp"""
    if len(p) == 2:
        p[0] = (p[1], None) # a (default value is 0)
    else:   
        p[0] = (p[1], p[3]) # a = 1

def p_exp_list(p):
    """exp : list"""
    p[0] = p[1]

def p_list(p):
    """list : LBRACK optargs RBRACK"""
    p[0] = p[2]

def p_stmt_listpop(p):
    """stmt : NAME DOT POP LPAREN exp RPAREN"""
    p[0] = ('ListF', 'Pop', p[1], p[5])

def p_stmt_listpush(p):
    """stmt : NAME DOT PUSH LPAREN exp RPAREN"""
    p[0] = ('ListF', 'Push', p[1], p[5])

def p_exp_listindex(p):
    """exp : NAME DOT INDEX LPAREN exp RPAREN"""
    p[0] = ('GetIndex', p[1], p[5])

def p_exp_listslice(p):
    """exp : NAME DOT SLICE LPAREN exp COMMA exp RPAREN"""
    p[0] = ('Slice', p[1], p[5], p[7])

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
        p[0] = (p[1], None) # a (default value is 0)
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

def p_stmt_rightincdec(p):
    """
    stmt : exp DEC
         | exp INC
    """
    if p[2] == '--':
        p[0] = ('DecR', p[1])
    elif p[2] == '++':
        p[0] = ('IncR', p[1 ])
    
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

def p_exp_error(p):
    """exp : error"""
    print("Ghalat expression mila hai.")
    quit()

# banao a, or banao a = 3 (initialization and creation of variables)
def p_makevar(p):
    """stmt : MAKE structargs"""
    p[0] = ('Made', p[2]) 

def p_assignment(p):
    """stmt : NAME EQUAL exp"""
    p[0] = ('=',p[1],p[3])

def p_assignment_error(p):
    """stmt : NAME EQUAL error"""
    print("Ghalat expression mila hai.")
    quit()

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

'''
def p_error(p):
    if p == None:
        print("None type parse tree p. A line is missing a ;") # causes problems for the next line
        quit()
    print(f"Syntax error in line {p.lineno}.")
    quit()
    return
'''
parser = yacc.yacc() # start parsing, yacc object created
isBroken = False
globalenv = (None, {}) # parent == None for globalenv, and empty initially
structdef = {}
funcdef = {}

def eeb(e, env): #evaluate expression binary
    operator = e[1]
    if e[0] == 'Group':
        return eeb(e[1], env)
    elif e[0] == 'CompareEXP':
        return eec(e, env)
    elif e[0] == 'FCall':
        retVal = functionCall(e, env)
        if retVal != None:
            return retVal
    elif e[0] == 'Dec':
        return eeb(e[1], env) - 1
    elif e[0] == 'Inc':
        return eeb(e[1], env) + 1
    elif operator == '+':
        return eeb(e[2], env) + eeb(e[3], env)
    elif operator == '-':
        return eeb(e[2], env) - eeb(e[3], env)
    elif operator == '*':
        return eeb(e[2], env) * eeb(e[3], env)
    elif operator == '/':
        denom = eeb(e[3], env)
        if denom == 0:
            raise ZeroDivisionError("Error: Division by 0.")
        num = eeb(e[2], env)
        if type(num) == int and type(denom) == int:
            return  num // denom
        else:
            return num / denom
    elif operator == '^':
        return abs(eeb(e[2]), env)** eeb(e[3], env)
    elif operator == '%':
        return eeb(e[2], env) % eeb(e[3], env)
    elif e[0] == 'Name':
        varname = e[1]
        if envLookup(env, varname) == None:
            raise NameError(f"Error: No such variable \'{varname}\' exists.")
        return envLookup(env, varname) 
    else:
        return e[1]

    
def eec(e, env): # evaluate expression comparison
    #print(f"CT in eec: {e}")
    operator = e[1]
    if e[0] == 'Group':
        return expEvaluate(e[1], env)
    elif e[0] == 'BinaryEXP':
        return eeb(e, env)
    elif operator == '<':
        boolres = eec(e[2], env) < eec(e[3], env)
    elif operator == '<=':
        boolres = eec(e[2], env) <= eec(e[3], env)
    elif operator == '>':
        boolres = eec(e[2], env) > eec(e[3], env)
    elif operator == '>=':
        boolres = eec(e[2], env) >= eec(e[3], env)
    elif operator == '!=':
        boolres = eec(e[2], env) != eec(e[3], env)
    elif operator == '==':
        boolres = eec(e[2], env) == eec(e[3], env)
    elif operator == 'aur':
        paramL = eec(e[2], env)
        paramR = eec(e[3], env)
        if type(paramL) == str or type(paramL) == int or type(paramL) == float:
            raise TypeError("Error: Type not supported with this operation.")
        elif type(paramR) == str or type(paramR) == int or type(paramR) == float:
            raise TypeError("Error: Type not supported with this operation.")
        boolres = paramL and paramR
    elif operator == 'ya':
        paramL = eec(e[2], env)
        paramR = eec(e[3], env)
        if type(paramL) == str or type(paramL) == int or type(paramL) == float:
            raise TypeError("Error: Type not supported with this operation.")
        elif type(paramR) == str or type(paramR) == int or type(paramR) == float:
            raise TypeError("Error: Type not supported with this operation.")
        boolres = paramL or paramR
    elif e[0] == 'Name':
        varname = e[1]
        varVal = envLookup(env, varname)
        if varVal == None:
            raise NameError(f"Error: No such variable \'{e[1]}\' exists.")
        return varVal
    elif e[0] == 'Dec':
        return eec(e[1], env) - 1
    elif e[0] == 'Inc':
        return eec(e[1], env) + 1
    else:
        boolres = e[1]

    return boolres


def varCreate(p, env): # variable will be created in the environment where varCreate is called
    #print(p)
    #print("VarCreate",env)
    # initialization with creation
    varname = p[0]
    if varname in env:
        raise KeyError(f"Error: A variable by that name already exists.")
    value = p[1]
    if type(value) == list:
        varlist = []
        for element in value:
            varlist.append(element[1]) # assuming not list inside, only single var
        env[1][varname] = varlist
    else:
        value = expEvaluate(p[1], env)
        #print("Var",varname, value)
        #print("Current tree in varCreate", p, "Value",value)
        # where p[1][1] will be the NAME (LHS), p[1][2] the expression to be evaluated (RHS)
        env[1][varname] = value

    #print(env)

def blockEvaluate(p, env): # p[0] == 'Block'
    global isBroken
    stmtlist = p[1]
    child_env = (env, {}) # create new env inside block everytime
    # make parent pointer of new child env point to previous env
    for stmt in stmtlist:
        #print(stmt, env)
            #quit()
        if stmt == 'Continue':
            break # breaking this iteration of the loop OR block later, so this is continue actually
        if stmt == 'Break':
            isBroken = True # this also breaks the next iterations/stmts, only loops turn this off after execution, so give error in normal blocks
            break # break this iteration
        if stmt != None and stmt[1] != []:
            #print(stmt)
            ret = stmtEvaluate(stmt, child_env)
            if ret != None:
                #print("Block got ret value",ret)
                return ret

def templateFunction(fname, stmtlist, env):
    global funcdef
    # currently every fargs stores every variable in 2-length tuples (NAME,VALUE)
    # need to convert to env dictionary form
    returnVal = blockEvaluate(stmtlist, env)    
    if returnVal != None:
        #print("function got retvalue", returnVal)
        return returnVal

counter = 0   
def functionCall(f, env):
    func = f[1]
    fargs = f[2]
    if not func in funcdef:
        raise NameError(f"Error: No such function \'{func}\' defined.")

    defined_fargs = funcdef[func][0]    
    if len(fargs) > len(defined_fargs):
        raise RuntimeError(f"Error: Function {func} was given too many arguments.")

    #mapped values of given fargs to defined fargs
    for i in range(len(fargs)): #dfa = defined function argument
        defined_fargs[i] = (defined_fargs[i][0], expEvaluate(fargs[i], env)) 
        
    #print("MAPPED FARGS:", defined_fargs)
    # more error handling for args, but assume same number of args for now
    #print(funcdef[func][1])
    fenv_dict = {}
    for fa in defined_fargs:
        fenv_dict[fa[0]] = fa[1]
    # fargs list converted to function environment of variables
    if env[0] == None:
        fenv = (env, fenv_dict)
    else:
        fenv = (env[0], fenv_dict)
    # converted to fenv
    #print(colored(f"New fenv in call {fenv}","blue"))
    global counter
    return templateFunction(func, funcdef[func][1], fenv)

# if None returned could not find any such variable, give error
def envLookup(env, varName):
    env_parent = env[0]
    env_dict = env[1]
    if varName in env_dict:
        #print('found',varName)
        if env_dict[varName] == None:
            return 0;
        else:
            return env_dict[varName]
    elif env_parent == None:
        return None
    else:
        return envLookup(env_parent, varName)

def envUpdate(env, varName, varVal):
    env_parent = env[0]
    if varName in env[1]:
        env[1][varName] = varVal
    elif env_parent != None:
        envUpdate(env_parent, varName, varVal)
        
def expEvaluate(e, env):
    global structdef
    global funcdef
    #print(f"exp: {e}")    
    if e == None: # none expression
        return None
    
    etype = e[0]
    if etype == 'BinaryEXP':
        return eeb(e, env) # evaluate binary expression
    elif etype == 'CompareEXP':
        return eec(e, env) # evaluate expression comparison
    elif etype == 'Group':
        return expEvaluate(e[1], env)
    elif etype == 'Name': # get value of variable
        varname = e[1]
        if envLookup(env, varname) == None:
            raise NameError(f"Error: No such variable \'{varname}\' exists.")
        return envLookup(env, varname)
    elif etype == 'FCall':
        return functionCall(e, env)
    elif etype == 'GetIndex':
        listvar = e[1]
        val = envLookup(env, listvar)
        if val != None and type(val) == list:
            index = eeb(e[2], env)
            try:
                return val[index]
            except:
                raise NameError("Error: List index out of range.") ###   
        else:
            raise NameError("Error: List error.")
    elif etype == 'Slice':
        listvar = e[1]
        val = envLookup(env, listvar)
        if val != None and type(val) == list:
            e1 = eeb(e[2], env) # evaluate expression, error handling
            e2 = eeb(e[3], env)
            return val[e1:e2+1]
        else:
            raise NameError("Error: List error.")
    elif etype == 'SVGet':
        structObj = e[1]
        varName = e[2]
        structVal = envLookup(env, structObj)
        if structVal == None:
            raise NameError(f"Error: No such variable \'{structObj}\' exists.") 
        
        if structVal[varName] == None:
            raise NameError(f"Error: No such variable \'{varName}\' exists inside that struct.") 
        varVal = structVal[varName]
        return varVal
    elif etype == 'Dec':
        return eeb(e[1], env) - 1
    elif etype == 'Inc':
        return eeb(e[1], env) + 1
    else: # Num, String, Char etc.
        return e[1] 

def runProgram(p): # p[0] == 'Program': a bunch of statements
    global globalenv
    proglist = p[1]  
    for stmt in proglist: # statements in proglist
        if stmt != None and stmt[1] != []:
            stmtEvaluate(stmt[1], globalenv) # since stmt[0] == 'Stmt' here in Program
            # these directly read program statements have the global env

def stmtEvaluate(p, env): # p is the parsed tree / program, evalStmts
    global structdef, funcdef, isBroken
    #print(f"stmt: {p}","Env: ",env)
    stype = p[0] # node type of parse tree
    if stype == 'DecR':
        varname = p[1][1]
        newVal = envLookup(env, varname)-1
        envUpdate(env, varname, newVal)
    elif stype == 'IncR':
        varname = p[1][1]
        newVal = envLookup(env, varname)+1
        envUpdate(env, varname, newVal)
    elif stype == '=': # assignment
        varname = p[1]
        varVal = expEvaluate(p[2], env)
        if envLookup(env, varname) == None:
            raise NameError(f"Error: No such variable \'{p[1]}\' exists.")
        envUpdate(env, varname, varVal) # make an entry for the variable in the globalenv dictionary, where the key is the NAME and the value is the assigned expression
    elif stype == 'Made': # creation of a variable
        for varg in p[1]:
            varCreate(varg, env)
    elif stype == 'If':    
        condition = p[1]
        then_stmts = p[2]
        #print(f"Condition: {condition} Then {then_stmts}")
        if eec(condition, env):
            #print("Running then statements in if")
            ret = blockEvaluate(then_stmts, env)
            if ret != None:
                return ret
    elif stype == 'IfElse':    
        condition = p[1]
        then_stmts = p[2]
        else_stmts = p[3]
        if eec(condition, env):
            ret = blockEvaluate(then_stmts, env)
            if ret != None:
                return ret
        else:
            ret = blockEvaluate(else_stmts, env)
            if ret != None:
                return ret
    elif stype == 'IfElifElse':
        condition = p[1]
        then_stmts = p[2]
        elif_stmts = p[3]
        else_stmts = p[4]
        if eec(condition, env):
            ret = blockEvaluate(then_stmts, env)
            if ret != None:
                return ret
        else:
            esTrue = False
            es_then = None
            for es in elif_stmts:
                es_condition = es[0]
                es_then = es[1]
                if eec(es_condition, env):
                    esTrue = True
                    break
            if esTrue and es_then != None:
                ret = blockEvaluate(es_then, env)
                if ret != None:
                    return ret
            else:
                ret = blockEvaluate(else_stmts, env)
                if ret != None:
                    return ret
    elif stype == 'For':
        countervar = p[1]
        old_counterval = envLookup(env, countervar)
        if old_counterval == None:
            raise NameError(f"Error: No such variable \'{countervar}\' exists.") 
        start = eeb(p[2], env) # must be numerical expression
        end = eeb(p[3], env) # evaluating numerical expressions
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
        
        for i in range(start, end+1, step):
            envUpdate(env, countervar, i)
            ret = blockEvaluate(do_stmts, env)
            if ret != None:
                return ret
            if isBroken:
                break
        isBroken = False # reset break
        envUpdate(env, countervar, old_counterval)
        
    elif stype == 'DoWhile':
        do_stmts = p[1]
        condition = p[2]
        ret = blockEvaluate(do_stmts, env) # first time the block is just executed
        if ret != None:
            return ret
        while eec(condition, env): # condition is necessary then onwards
            if isBroken:
                break
            ret = blockEvaluate(do_stmts, env)
            if ret != None:
                return ret
        isBroken = False
    elif stype == 'Print':
        printList = p[1]
        for element in printList:
            result = expEvaluate(element, env)
            if type(result) == bool:
                if result == True:
                    result = 'sach'
                elif result == False:
                    result = 'ghalat'
            print(result, end=" ")
        print()  
    elif stype == 'FCall':
        functionCall(p, env)
        # nothing is returned in the simple void stmt function call
    elif stype == 'FReturn':
        #print(colored(f"Inside a return statment with exp {p}","green"))
        returnVal = expEvaluate(p[1], env)
        #print(colored(f"Returned value: {returnVal}","red"))
        #quit()
        if returnVal != None:
            return returnVal

    elif stype == 'Func':
        func = p[1]
        fargs = p[2]
        fstmts = p[3]
        if func in funcdef:
            raise KeyError(f"Function {func} has already been defined.")
        
        # evaluate fargs values further
        for i in range(len(fargs)):
            fargs[i] = (fargs[i][0], expEvaluate(fargs[i][1], env))

        funcdef[func] = [fargs, fstmts] # default values of args, fargs must be resolved further
    elif stype == 'ListF':
        listvar = p[2]
        val = envLookup(env, listvar)
        if val != None and type(val) == list:
            ftype = p[1]
            e1 = eeb(p[3], env) # evaluate expression, error handling
            if ftype == 'Pop':
                if e1 == 0: # remove head of list
                    del val[0]
                elif e1 == 1: # remove tail of list
                    val.pop()
            elif ftype == 'Push': #append
                val.append(e1)
            envUpdate(env, listvar, val)
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
            structdef[structname][varName] = expEvaluate(varVal, env) # varVal may be None here.
    
    elif stype == 'SMade':
        structname = p[1]
        varName = p[2] # object variable
        if not structname in structdef:
            raise NameError(f"Error: No struct of that name is defined.")
        
        val = envLookup(env, varName)
        if val != None:
            raise KeyError(f"Error: This variable already exists.") 
        
        env[1][varName] = structdef[structname]
    
    elif stype == 'SVAssign':
        structObj = p[1]
        varName = p[2]
        varVal = p[3]
        sVal = envLookup(env, structObj)
        if sVal == None:
            raise NameError(f"Error: No such variable \'{structObj}\' exists.") 
        
        if not varName in sVal:
            raise NameError(f"Error: No such variable \'{varName}\' exists inside that struct.") 
        
        sVal[varName] = expEvaluate(varVal, env) # no need to envUpdate, since this points to same dict
    
    #print("GLOBAL ENV:",globalenv)    

def printWithSyntaxHighlights(userin, colorstr):
    proglines = userin.split('\n')
    progCIlist = []
    for line in proglines:
        wordlist = line.split(' ')
        kwlist = []
        for word in wordlist:
            for resword in reserved:
                if resword in word and line[0] != "~":
                    kw = [line.index(resword), line.index(resword)+len(resword)-1]
                    kwlist.append(kw)
        
        colorIndexes = []
        for rang in kwlist:
            for i in range(rang[0],rang[1]+1):
                if i not in colorIndexes:
                    colorIndexes.append(i)
        progCIlist.append(colorIndexes)

    i = 0
    for line in proglines:
        j = 0
        for char in line:
            if j in progCIlist[i]:
                print(colored(char,colorstr), end= "")
            else:
                print(char, end="")
            j += 1
        print()
        i+=1


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
    globalenv = (None, {}) #refresh state
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
    
    printWithSyntaxHighlights(userin, "yellow")
    print("=========================================\n{OUTPUT}")
    try:
        runProgram(('Program',uzparsed))
    except Exception as e:
        print(e)
    
    input("Press any key to run code again.")


exit()