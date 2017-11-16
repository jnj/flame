# flameparse.py
# the Flame parser module
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: flameparse.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $	


# Modules to load
import pyson
import flamelex
import symtab
import flameerr
import flametype


# Set whether or not the error printing
# is checked for duplicate messages
flameerr.flerror_quiet = None


# Use the list of tokens defined in flamelex.py
tokens = flamelex.tokens


# Set up the precedence rules for operators
precedence = (
    ('left', 'AND'),
    ('left', 'OR'),
    ('right', 'NOT'),
    ('left', 'LT', 'LEQ', 'GT', 'GEQ', 'EQUALS', 'NEQUAL'),
    ('left', 'ASSIGN'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS', 'UPLUS')
    )


# Define a Node in the parse tree
class Node:
    def __init__(self, type, children, value=None):
        self.type = type
        self.children = children
        self.value = value


# This function is used for concatenating adjacent strings.
# The strings  themselves  are  grouped by the p_strings*()
# functions later  in  this file. The  argument passed is a
# Node whose value is the first string and whose children's
# value attributes contain the rest of the strings.
def concat(t):
    u = t.value[:-1]
    v = reduce(lambda x,y: x+y, map(lambda n: n.value[1:-1], t.children), "")
    return u + v + '"'



# This function checks that the types of the grammar symbols
# passed  to  it  are  the same. If so, it returns the type.
# Otherwise, it returns the error type flameerr.errtype (all
# types are two-element tuples).
def checktypes(s, t):
    try:
        if s.datatype[0] == 'error' or t.datatype[0] == 'error':
            # At least one of them has error type
            return flameerr.errtype

        elif s.datatype == t.datatype:
            # Everything is ok
            return s.datatype

        else:
            # Different types
            return flameerr.errtype

    except AttributeError:
        return flameerr.errtype


# This function checks if an identifier has been redeclared.
# If it is has been redeclared, returns 1. Otherwise, returns 0.
def isredecl(t):
    if t.symtab.datatype:
        return 1
    return 0


# This function checks if an identifier is undeclared. If it is,
# returns 1. Otherwise, returns 0. SIDE EFECT: attaches symtab
# entry to t if it is found.
def isundecl(t):
    # Try to look it up locally. Everything should
    # return a local entry. If the local entry is
    # blank, then we need to look at the global table.
    lentry = symtab.local_lookup(t.value)
    if not lentry or not lentry.datatype:
        gentry = symtab.global_lookup(t.value)

        if not gentry:
            return 1

        if not gentry.datatype:
            # Must be a function
            t.symtab = gentry
            return 0

        # Variable or function found globally
        tmp = t.symtab
        t.symtab = gentry
        del tmp
        return 0

    t.symtab = lentry
    return 0


# This function searches a statements subtree (Node) for
# a "break" not inside a "while". Returns 1 if yes,
# otherwise 0. Note: This is much easier than trying to
# propagate some symbol up through the tree.
def getbadbreaks(t):
    # Maintain a list of line numbers where they occur
    breaks = []
    # Recursively search the tree
    for i in t.children:
        if i.type == 'if':
            if i.children[1].type == 'statement_block':
                breaks += getbadbreaks(i.children[1].children[0])
            elif i.children[1].type == 'break':
                breaks.append(i.children[1].value)

            # Check THEN clause
            l = len(i.children)
            if l > 2:
                if i.children[2].type == 'statement_block':
                    breaks += getbadbreaks(i.children[2].children[0])
                elif i.children[2].type == 'break':
                    breaks.append(i.children[2].value)
            else:
                pass

        elif i.type == 'break':
            breaks.append(i.value)

    return breaks


# This function examines the statements tree of a function for
# things like some, but not all branches returning or no branches
# returning. Meanings of return values:
# 2 - function returns ok
# 1 - function returns from some branches, but not others
# 0 - function does not return (is void)
# This is recursive, so 'function' above applies to statements as well
def examine(t):
    # If there is a return off the main trunk of the statements
    # tree, then the function returns no matter what
    if filter(lambda node: node.type == 'return', t.children) != []:
        # The function returns
        return 2
    else:
        # Ok, we couldn't find any return statements on the main
        # trunk of the statements tree -- we now need to examine each
        # branch off of the main tree. Most statements don't cause
        # a branch in program flow, but a few do: if, if-else, while,
        # and begin...end blocks. So we need to find these.
        # Define a filtering function for find branch statements
        def ff(node):
            if node.type == 'statement_block' or \
               node.type == 'if' or \
               node.type == 'while':
                return 1
            else:
                return 0

        # Find the branches and count them
        branches = filter(ff, t.children)
        nbranches = len(branches)

        # We want to have 1 item for each branch
        # Here's a quick way to populate the list        
        nreturns = [0] * nbranches

        # If it doesn't have any branches, then it's void
        # and we are done
        if nreturns == []:
            return 0
        
        # Consider each branch statement
        for i in range(nbranches):
            br = branches[i]

            # Statement block
            if br.type == 'statement_block':
                nreturns[i] = examine(br.children[0])

            # While statement
            elif br.type == 'while':
                if br.children[1].type == 'return':
                    nreturns[i] = 1
                elif br.children[1].type == 'statement_block':
                    a = examine(br.children[1].children[0])
                    if a > 0:
                        nreturns[i] = 1
            # If statement
            elif br.type == 'if':
                # How many branches of the if statement return
                ifreturns = 0

                # Check for if-else
                if len(br.children) > 2:
                    ifelse = 1
                else:
                    ifelse = 0
                
                # First consider the 'if' branch
                if br.children[1].type == 'return':
                    ifreturns += 1
                elif br.children[1].type == 'statement_block':
                    if examine(br.children[1].children[0]) > 1:
                        ifreturns += 1 
                else:
                    pass

                # If it's not an if-else and the 'if' returns,
                # then we're done here
                if not ifelse:
                    if ifreturns:
                        return 1
                    else:
                        return 0

                # Consider the 'else' branch if there is one
                else:
                    if br.children[2].type == 'return':
                        ifreturns += 1
                    elif br.children[2].type == 'statement_block':
                        if examine(br.children[2].children[0]) > 1:
                            ifreturns += 1
                    else:
                        pass

                # Return the appropriate value
                if ifelse:
                    if ifreturns > 1:
                        nreturns[i] = 2
                    elif ifreturns > 0:
                        nreturns[i] = 1
                else:
                    if ifreturns:
                        nreturns[i] = 2

        # Check if each branch returns
        # and return the appropriate value
        if nreturns.count(2) == nbranches:
            return 2
        elif nreturns.count(1) > 0:
            return 1
        else:
            return 0     


# infertypes()
# A function to handle the inference logic in binary operations.
# In the call, a would be t[0], b t[1] and c t[3] (t[2] is the operator)
def infertypes(a, b, c):
    st = flametype.settype

    # First is a function and second is not
    if hasattr(b, 'function') and not hasattr(c, 'function'):
        # If b has no type, infer it from type of c
        if b.datatype == (None, None):
            t = st(b.function, c.datatype[0])
            b.datatype = (t.type, None)

    # First is not a function and second is
    if hasattr(c, 'function') and not hasattr(b, 'function'):
        # If c has no type, infer it from type of b
        if c.datatype == (None, None):
            t = st(c.function, b.datatype[0])
            c.datatype = (t.type, None)
    a.datatype = checktypes(b, c)

    # Both are function calls
    if hasattr(b, 'function') and hasattr(c, 'function'):
        a.function = b.function
        if a.datatype != flameerr.errtype:
            flametype.unify(b.function, c.function)


# The following functions are the rules for parsing the grammar.
# Each function corresponds to a production in the grammar.
def p_program(t):
    'program : fundecls'

    # Stick the maximum nesting level as the root Node value
    t[0].value = Node('program', [t[1].value], symtab.maxnest)
    t[0].lineno = t[1].lineno

    # symtab.dump_all()
    t[0].value.symtab = symtab.pop_table()

    # Spit out the types we determined (for debugging)
    #flametype.dump()
    
    # Report some errors involving function types and program flow
    flametype.reportvoids()
    flametype.reportnones()

    # Check for main being defined
    if not flametype.types.has_key('main'):
        # flameerr.flerror(1, 60, ())
        flameerr.nocompile = 1

    # If any errors have occurred, we can end compilation here
    if flameerr.nocompile:
        t[0].value = None


def p_fundecls_fun(t):
    'fundecls : fundecl'
    t[0].value = Node('functions', [t[1].value])
    t[0].lineno = t[1].lineno 


def p_fundecls(t):
    'fundecls : fundecls fundecl'
    t[0].value = t[1].value
    t[0].value.children.append(t[2].value)
    t[0].lineno = t[2].lineno 


def p_fundecl(t):
    'fundecl : fun_begin locals BEGIN statements END'
    children = [t[1].value, t[2].value, t[4].value]
    t[0].value = Node('function_definition', children)
    t[0].lineno = t[5].lineno

    # Report instances of misplaced breaks
    for i in getbadbreaks(t[4].value):
        flameerr.flerror(i, 34, ())

    # Exmaine program flow paths for errors
    e = examine(t[4].value)

    if e == 1:
        # Function only returns from some branches and not others
        line = flameerr.funlines[flametype.getfunction()]
        flameerr.flerror(line, 53, (t[1].value.value))

    elif e == 0:
        # Function is void
        flametype.settype(flametype.getfunction(), 'void')

    # Update the current function
    flametype.closefunction()

    # Restore the old symbol table
    t[0].value.symtab = symtab.pop_table()


def p_fundecl_error_empty_block(t):
    'fundecl : fun_begin locals BEGIN END'
    flameerr.flerror(t[3].lineno, 10, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_fun_begin(t):
    'fun_begin : fun ID LPAREN funargs RPAREN'
    flameerr.funlines[t[2].value] = t[2].lineno
    flametype.openfunction(t[2].value)

    # Check for a redeclared function
    if isredecl(t[2]):
        flameerr.flerror(t[2].lineno, 56, (t[2].value))
        t[2].symtab.datatype = flameerr.errtype
    else:
        t[2].symtab.datatype = flametype.gettype(t[2].value)
        t[2].symtab.isfunction = 1
        # Fill in the argument types (don't know return type yet)
        t[2].symtab.argtypes = t[4].datatype
        t[2].symtab.argcount = len(t[4].datatype)

    t[0].value = Node('function_start', [t[4].value], t[2].value)
    t[0].lineno = t[2].lineno
    t[0].funid = t[2]


def p_fun(t):
    'fun : FUN'
    # Create a new symbol table
    symtab.push_table()
    symtab.add_new_symbol('$maxargs')
    symtab.add_new_symbol('$nestlev')
    symtab.set_attr('$maxargs', 'size', 0)
    symtab.set_attr('$nestlev', 'size', symtab.get_nestlev())

def p_funargs(t):
    'funargs : arglist'
    t[0].value = Node('function_arguments', [t[1].value])
    t[0].lineno = t[1].lineno
    t[0].datatype = tuple(t[1].datatype)
    t[0].value.datatype = t[0].datatype


def p_funargs_empty(t):
    'funargs : empty'
    t[0].value = Node('function_arguments', [])
    t[0].datatype = ()
    t[0].value.datatype = t[0].datatype
    

def p_arglist(t):
    'arglist : arglist COMMA arg'
    t[0].value = t[1].value
    t[0].value.children.append(t[3].value)
    t[0].lineno = t[3].lineno
    t[0].datatype = t[1].datatype
    t[0].datatype.append(t[3].datatype)
    t[0].value.datatype = t[0].datatype


def p_arglist_arg(t):
    'arglist : arg'
    t[0].value = Node('arguments_list', [t[1].value])
    t[0].lineno = t[1].lineno
    t[0].datatype = [t[1].datatype]
    t[0].value.datatype = t[0].datatype
    

def p_arg(t):
    'arg : ID COLON type'
    # Check for redeclared argument
    if isredecl(t[1]):
        flameerr.flerror(t[1].lineno, 49, t[1].value)
        t[1].symtab.datatype = flameerr.errtype
    else:
        t[1].symtab.datatype = t[3].datatype
        t[1].symtab.nestlev = symtab.get_nestlev()
    t[0].value = Node('argument', [t[3].value], t[1].value)
    t[0].lineno = t[3].lineno
    t[0].datatype = t[1].symtab.datatype
    t[0].value.datatype = t[0].datatype

    # Bad array
    if t[3].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 32, (t[1].value))


def p_locals(t):
    'locals : locallist'
    t[0].value = Node('function_locals', [t[1].value])
    t[0].lineno = t[1].lineno


def p_locals_empty(t):
    'locals : empty'
    t[0].value = Node('function_locals', [])


def p_locallist(t):
    'locallist : locallist local'
    t[0].value = t[1].value
    t[0].value.children.append(t[2].value)
    t[0].lineno = t[2].lineno


def p_locallist_local(t):
    'locallist : local'
    t[0].value = Node('locals_list', [t[1].value])
    t[0].lineno = t[1].lineno


def p_local(t):
    'local : localdecl SEMI'
    t[0].value = Node('local', [t[1].value])
    t[0].lineno = t[2].lineno


def p_local_error_1(t):
    'local : localdecl error'
    flameerr.flerror(t[1].lineno, 57, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno


def p_local_var(t):
    'localdecl : ID COLON type'
    # Check for redeclared variable
    if isredecl(t[1]):
        flameerr.flerror(t[1].lineno, 49, t[1].value)
        t[1].symtab.datatype = flameerr.errtype
    else:
        t[1].symtab.datatype = t[3].datatype
        # Attach its nesting level
        t[1].symtab.nestlev = symtab.get_nestlev()
    t[0].value = Node('local_variable', [t[3].value], t[1].value)
    t[0].lineno = t[3].lineno
    t[0].datatype = t[1].symtab.datatype
    t[0].value.datatype = t[0].datatype
    
    # Bad array
    if t[3].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 32, (t[1].value))


def p_local_var_error(t):
    'localdecl : ID COLON error'
    flameerr.flerror(t[3].lineno, 58, (t[3].value.value))
    t[0].value = Node('error', [])
    t[0].lineno = t[3].lineno


def p_local_var_array_error(t):
    'localdecl : ID COLON error LBRACE'
    flameerr.flerror(t[3].lineno, 58, (t[3].value.value))
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno
    

def p_local_fun(t):
    'localdecl : fundecl'
    t[0].value = Node('local_function', [t[1].value])
    t[0].lineno = t[1].lineno
    

def p_statements(t):
    'statements : statements SEMI statement'
    t[0].value = t[1].value
    t[0].value.children.append(t[3].value)
    t[0].lineno = t[3].lineno


def p_statements_statement(t):
    'statements : statement'
    t[0].value = Node('statements', [t[1].value])
    t[0].lineno = t[1].lineno


def p_statement_misplaced_decl_var(t):
    'statement : ID COLON error'
    flameerr.flerror(t[1].lineno, 11, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[1].lineno


def p_statement_while(t):
    'statement : WHILE relation DO statement'
    if hasattr(t[2], 'eval'):
        if t[2].eval == 1:
            # WHILE body always executes
            pass
        elif t[2].eval == 0:
            # WHILE body never executes -- use a SKIP
            t[0].value = Node('skip', [])
            t[0].lineno = t[1].lineno
            return

    t[0].value = Node('while', [t[2].value, t[4].value])
    t[0].lineno = t[4].lineno


def p_statement_while_error(t):
    'statement : WHILE error DO statement'
    flameerr.flerror(t[2].lineno, 12, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_statement_if(t):
    'statement : IF relation THEN statement'
    if hasattr(t[2], 'eval'):
        # Constant relation
        if t[2].eval == 1:
            # IF body always executes
            # Skip the IF and just add the statements to the tree
            t[0].value = t[4].value
            t[0].lineno = t[4].lineno
        else:
            # IF body never executes -- use a SKIP
            t[0].value = Node('skip', [])
            t[0].lineno = t[1].lineno
        return
    else:
        # Non-constant relation
        t[0].value = Node('if', [t[2].value, t[4].value])
        t[0].lineno = t[4].lineno


def p_statement_if_error(t):
    'statement : IF error THEN statement'
    flameerr.flerror(t[2].lineno, 13, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_statement_misplaced_else(t):
    'statement : error ELSE error'
    flameerr.flerror(t[1].lineno, 14, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[1].lineno


def p_statement_ifelse(t):
    'statement : IF relation THEN statement ELSE statement'
    if hasattr(t[2], 'eval'):
        # Constant relation
        if t[2].eval == 1:
            # IF body always executes
            # Skip the IF and just put the statements in the tree
            t[0].value = t[4].value
            t[0].lineno = t[4].lineno
        else:
            # Else body always executes
            # The ELSE statements always get executed, so use them directly
            t[0].value = t[6].value
            t[0].lineno = t[6].lineno
    else:
        # The relation was not constant
        t[0].value = Node('if', [t[2].value, t[4].value, t[6].value])
        t[0].lineno = t[6].lineno


def p_statement_assign(t):
    'statement : location ASSIGN expression'
    infertypes(t[0], t[1], t[3])
    if hasattr(t[1], 'noindex'):
        t[0].datatype = flameerr.errtype
        flameerr.flerror(t[1].noindex, 30, ())
    else:
        t[0].datatype = checktypes(t[1], t[3])
        if t[0].datatype == flameerr.errtype:
            flameerr.flerror(t[2].lineno, 30, ())

    t[0].value = Node('assignment', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno


def p_statement_assign_error_1(t):
    'statement : location ASSIGN error'
    flameerr.flerror(t[3].lineno, 15, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[3].lineno


def p_statement_assign_error_2(t):
    'statement : location ASSIGN error END'
    flameerr.flerror(t[3].lineno, 15, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_statement_print(t):
    'statement : PRINT LPAREN literal RPAREN'
    t[0].value = Node('print', [t[3].value])
    t[0].lineno = t[4].lineno


def p_statement_print_error(t):
    'statement : PRINT LPAREN error RPAREN'
    flameerr.flerror(t[3].lineno, 16, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_statement_write(t):
    'statement : WRITE LPAREN expression RPAREN'
    t[0].value = Node('write', [t[3].value], t[3].datatype[0])
    t[0].lineno = t[4].lineno
    # Check for unindexed array
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[3].lineno, 36, ())
    # Check the expression
    if t[3].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 36, ())


def p_statement_write_error(t):
    'statement : WRITE LPAREN error RPAREN'
    flameerr.flerror(t[3].lineno, 17, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_statement_read(t):
    'statement : READ LPAREN location RPAREN'
    t[0].value = Node('read', [t[3].value], t[3].datatype[0])
    t[0].lineno = t[4].lineno
    # Check for unindexed array
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[3].lineno, 35, ())
    # Check the expression
    if t[3].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 35, ())


def p_statement_read_error(t):
    'statement : READ LPAREN error RPAREN'
    flameerr.flerror(t[3].lineno, 18, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno
    

def p_statement_return(t):
    'statement : RETURN expression'
    t[0].value = Node('return', [t[2].value], t[1].lineno)
    t[0].lineno = t[2].lineno
    # Check for unindexed array
    if hasattr(t[2], 'noindex'):
        flameerr.flerror(t[2].lineno, 37, ())
    # Check the expression
    if t[2].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 37, ())
    # Check the return type against the function we're in
    f = flametype.getfunction()
    funtype = flametype.gettype(f).type
    if funtype:
        if t[2].datatype[0] != funtype:
            flameerr.flerror(t[1].lineno, 48, ())
    else:
        if t[2].datatype != flameerr.errtype:
            flametype.settype(f, t[2].datatype[0])
                              

def p_statement_return_error_1(t):
    'statement : RETURN error SEMI'
    flameerr.flerror(t[1].lineno, 19, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno


def p_statement_return_error_2(t):
    'statement : RETURN error END'
    flameerr.flerror(t[1].lineno, 19, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno
    

def p_statement_skip(t):
    'statement : SKIP'
    t[0].value = Node('skip', [])
    t[0].lineno = t[1].lineno


def p_statement_break(t):
    'statement : BREAK'
    t[0].value = Node('break', [], t[1].lineno)
    t[0].lineno = t[1].lineno
    t[0].hasbreak = 1


def p_statement_statements(t):
    'statement : BEGIN statements END'
    t[0].value = Node('statement_block', [t[2].value])
    t[0].lineno = t[3].lineno
    if hasattr(t[2], 'returntype'):
        t[0].returntype = t[2].returntype


def p_statement_statements_error(t):
    'statement : BEGIN END'
    flameerr.flerror(t[2].lineno, 10, ())
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno


def p_statement_funcall(t):
    'statement : ID LPAREN exprs RPAREN'
    t[0].datatype = flameerr.errtype
    t[0].value = Node('function_call', [t[3].value], t[1].value)
    t[0].lineno = t[4].lineno
    # Check if the function is undeclared
    if isundecl(t[1]):
        flameerr.flerror(t[1].lineno, 51, (t[1].value))
        t[1].symtab.datatype = flameerr.errtype
        t[0].datatype = t[1].symtab.datatype
        return
    # Make sure ID is a function
    if not hasattr(t[1].symtab, 'isfunction'):
        flameerr.flerror(t[1].lineno, 59, (t[1].value))
        t[1].symtab.datatype = flameerr.errtype
        t[0].datatype = t[1].symtab.datatype
        return
    # Check no. arguments
    nr_args = t[1].symtab.argcount
    maxargs = symtab.get_attr('$maxargs', 'size')
    symtab.set_attr('$maxargs', 'size', max(nr_args, maxargs))

    if nr_args != t[3].len:
        flameerr.flerror(t[2].lineno, 29, (t[1].value))
    # Check argument types
    for i in range(t[3].len):
        if nr_args <= i:
            break
        if t[1].symtab.argtypes[i] != t[3].datatype[i]:
            # Type mismatch in function and call
            flameerr.flerror(t[3].lineno, 28, (i+1, t[1].value))


def p_relation_relation_and(t):
    'relation : relation AND relation'
    if hasattr(t[1], 'eval') and hasattr(t[3], 'eval'):
        # Constant relation
        if t[1].eval == 1 and t[3].eval == 1:
            t[0].eval = 1
        else:
            t[0].eval = 0
    t[0].value = Node('and', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_relation_or(t):
    'relation : relation OR relation'
    if hasattr(t[1], 'eval'):
        if t[1].eval == 1:
            t[0].eval = 1
    elif hasattr(t[3], 'eval'):
        if t[3].eval == 1:
            t[0].eval = 1
    t[0].value = Node('or', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno


def p_relation_and(t):
    'relation : expression AND expression'
    if hasattr(t[1], 'eval') and hasattr(t[3], 'eval'):
        # Constant relation
        if t[1].eval == 1 and t[3].eval == 1:
            t[0].eval = 1
        else:
            t[0].eval = 0
    t[0].value = Node('and', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_or(t):
    'relation : expression OR expression'
    if hasattr(t[1], 'eval'):
        if t[1].eval == 1:
            t[0].eval = 1
    elif hasattr(t[3], 'eval'):
        if t[3].eval == 1:
            t[0].eval = 1
    t[0].value = Node('or', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno


def p_relation_lt(t):
    'relation :  expression LT expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 44, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval < t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 0
    t[0].value = Node('<', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_gt(t):
    'relation : expression GT expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 45, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval > t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 0
    t[0].value = Node('>', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_leq(t):
    'relation : expression LEQ expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 46, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval <= t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 1                
    t[0].value = Node('<=', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_geq(t):
    'relation : expression GEQ expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 47, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval >= t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 1
    t[0].value = Node('>=', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno


def p_relation_eq(t):
    'relation : expression EQUALS expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 43, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval == t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 1
    t[0].value = Node('==', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno


def p_relation_neq(t):
    'relation : expression NEQUAL expression'
    t[0].datatype = checktypes(t[1], t[3])
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[2].lineno, 42, ())
    else:
        if t[1].eval != 'error' and t[3].eval != 'error':
            # Constant relation
            if t[1].eval != t[3].eval:
                t[0].eval = 1
            else:
                t[0].eval = 0
        elif hasattr(t[1], 'name') and hasattr(t[3], 'name') and \
             t[1].name == t[3].name:
            t[0].eval = 0
    t[0].value = Node('!=', [t[1].value, t[3].value])
    t[0].lineno = t[3].lineno
    

def p_relation_nrel(t):
    'relation : NOT relation'
    t[0].value = Node('not', [t[2].value])
    t[0].lineno = t[2].lineno
    if hasattr(t[2], 'eval'):
        # Constant relation
        if t[2].eval == 0:
            t[0].eval = 1
        else:
            t[0].eval = 0


def p_relation_prel(t):
    'relation : LPAREN relation RPAREN'
    t[0].value = t[2].value
    t[0].lineno = t[3].lineno
    if hasattr(t[2], 'eval'):
        t[0].eval = t[2].eval


def p_location_id(t):
    'location : ID'
    # Check for undeclared variable
    if isundecl(t[1]):
        flameerr.flerror(t[1].lineno, 50, (t[1].value))
        t[1].symtab.datatype = flameerr.errtype
    # Check for non-indexed array
    if t[1].symtab.datatype[1]:
        t[0].noindex = t[1].lineno
    else:
        t[0].datatype = t[1].symtab.datatype
    t[0].value = Node('location', [], t[1].value)
    t[0].lineno = t[1].lineno
    t[0].datatype = t[1].symtab.datatype
    t[0].name = t[1].value


def p_location_idexpr(t):
    'location : ID LBRACE expression RBRACE'
    if isundecl(t[1]):
        flameerr.flerror(t[1].lineno, 50, (t[1].value))
        t[0].value = Node('error', [])
        t[0].datatype = flameerr.errtype
        return
    t[0].datatype = (t[1].symtab.datatype[0], None)
    t[0].value = Node('location', [t[3].value], t[1].value)
    t[0].lineno = t[4].lineno
    t[0].isarray = 1
    # Make sure index is an integer
    if t[3].datatype[0] != 'int':
        flameerr.flerror(t[3].lineno, 32, (t[1].value))
        t[0].datatype = flameerr.errtype
    # Check array index bounds
    if t[3].eval != 'error' and t[3].eval < 0:
        flameerr.flerror(t[3].lineno, 31, ())
        t[0].datatype = flameerr.errtype
    # Check for non-array variable with index
    if not t[1].symtab.datatype[1]:
        flameerr.flerror(t[1].lineno, 33, (t[1].value))
        t[0].datatype = flameerr.errtype
    if t[3].eval != 'error':
        t[0].name = t[1].value + str(t[3].eval)
    else:
        t[0].name = t[1].value + str(t[3])


def p_array_error(t):
    'location : ID LBRACE error RBRACE'
    # Bad expression in array index
    flameerr.flerror(t[3].lineno, 22, (t[1].value))
    t[0].value = Node('error', [])
    t[0].lineno = t[4].lineno


def p_exprs(t):
    'exprs : exprlist'
    t[0].value = Node('expressions', [t[1].value])
    t[0].lineno = t[1].lineno
    try:
        t[0].len = t[1].len
        t[0].datatype = tuple(t[1].datatype)

    except AttributeError:
        pass


def p_exprs_empty(t):
    'exprs : empty'
    t[0].value = Node('expressions', [])
    t[0].len = 0
    t[0].datatype = ()


def p_exprlist(t):
    'exprlist : exprlist COMMA expression'
    t[0].value = t[1].value
    t[0].value.children.append(t[3].value)
    t[0].lineno = t[3].lineno
    try:
        t[0].len = t[1].len + 1
        t[0].datatype = t[1].datatype
        t[0].datatype.append(t[3].datatype)

    except AttributeError:
        pass


def p_exprlist_error(t):
    'exprlist : exprlist error expression'
    # Missing ',' in expression list
    flameerr.flerror(t[1].lineno, 23)
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno


def p_exprlist_expr(t):
    'exprlist : expression'
    t[0].datatype = [t[1].datatype]
    t[0].value = Node('expression_list', [t[1].value])
    t[0].lineno = t[1].lineno
    t[0].len = 1


def p_expression_plus(t):
    'expression : expression PLUS expression'
    infertypes(t[0], t[1], t[3])    
    if hasattr(t[1], 'noindex'):
        flameerr.flerror(t[1].lineno, 38, ())
        t[0].datatype = flameerr.errtype
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[1].lineno, 38, ())
        t[0].datatype = flameerr.errtype
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 38, ())
    t[0].lineno = t[3].lineno
    if t[1].eval != 'error' and t[3].eval != 'error':
        t[0].eval = t[1].eval + t[3].eval
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    elif hasattr(t[1], 'name') and t[3].eval == 0:
        t[0].eval = 'error'
        t[0].name = t[1].name
        t[0].value = t[1].value
    elif hasattr(t[3], 'name') and t[1].eval == 0:
        t[0].eval = 'error'
        t[0].name = t[3].name
        t[0].value = t[3].value
    else:
        t[0].eval = 'error'
        t[0].value = Node('plus', [t[1].value, t[3].value])


def p_expression_minus(t):
    'expression : expression MINUS expression'
    infertypes(t[0], t[1], t[3])
    if hasattr(t[1], 'noindex'):
        flameerr.flerror(t[1].lineno, 41, ())
        t[0].datatype = flameerr.errtype
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[1].lineno, 41, ())
        t[0].datatype = flameerr.errtype
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 41, ())
    t[0].lineno = t[3].lineno
    if t[1].eval != 'error' and t[3].eval != 'error':
        t[0].eval = t[1].eval - t[3].eval
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    elif hasattr(t[1], 'name') and hasattr(t[3], 'name'):
        # This does the evaluation for constant expressions like (a - a)
        if t[1].name == t[3].name and \
           not hasattr(t[1], 'isarray') and \
           not hasattr(t[3], 'isarray'):
            if t[1].datatype == ('int', None) and \
               t[3].datatype == ('int', None):
                t[0].eval = 0
                t[0].value = Node('int', [], t[0].eval)
            elif t[1].datatype == ('float', None) and \
                 t[3].datatype == ('float', None):
                t[0].eval = 0.0
                t[0].value = Node('float', [], t[0].eval)
        else:
            t[0].value = Node('minus', [t[1].value, t[3].value])
            t[0].eval = 'error'
    elif hasattr(t[1], 'name') and t[3].eval == 0:
        t[0].eval = 'error'
        t[0].name = t[1].name
        t[0].value = t[1].value
    else:
        t[0].eval = 'error'
        t[0].value = Node('minus', [t[1].value, t[3].value])
    

def p_expression_times(t):
    'expression : expression TIMES expression'
    infertypes(t[0], t[1], t[3])
    if hasattr(t[1], 'noindex'):
        flameerr.flerror(t[1].lineno, 39, ())
        t[0].datatype = flameerr.errtype
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[1].lineno, 39, ())
        t[0].datatype = flameerr.errtype
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 39, ())
    t[0].lineno = t[3].lineno
    if t[1].eval != 'error' and t[3].eval != 'error':
        t[0].eval = t[1].eval * t[3].eval
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    # The next two elifs simplify stuff like n * 1 or 1 * n or 0 * n
    elif t[3].eval == 1:
        if hasattr(t[1], 'name'):
            t[0].name = t[1].name
        t[0].eval = 'error'
        t[0].value = t[1].value
    elif t[1].eval == 1:
        if hasattr(t[3], 'name'):
            t[0].name = t[3].name
        t[0].eval = 'error'
        t[0].value = t[3].value
    elif hasattr(t[3], 'name') and t[1].eval == 1:
        t[0].eval = 'error'
        t[0].name = t[3].name
        t[0].value = t[3].value
    elif hasattr(t[1], 'name') and t[3].eval == 0:
        t[0].eval = 0
        t[0].value = Node(t[0].datatype[0], [], 0)
    elif hasattr(t[3], 'name') and t[1].eval == 0:
        t[0].eval = 0
        t[0].value = Node(t[0].datatype[0], [], 0)
    else:
        t[0].eval = 'error'
        t[0].value = Node('times', [t[1].value, t[3].value])


def p_expression_divide(t):
    'expression : expression DIVIDE expression'
    infertypes(t[0], t[1], t[3])
    if hasattr(t[1], 'noindex'):
        flameerr.flerror(t[1].lineno, 40, ())
        t[0].datatype = flameerr.errtype
    if hasattr(t[3], 'noindex'):
        flameerr.flerror(t[1].lineno, 40, ())
        t[0].datatype = flameerr.errtype
    if t[0].datatype == flameerr.errtype:
        flameerr.flerror(t[3].lineno, 40, ())
    t[0].lineno = t[3].lineno
    if t[3].eval == 0:
        # Divide by zero
        flameerr.flerror(t[3].lineno, 55, ())
        t[0].eval = 'error'
        t[0].value = Node('error', [])
        return
    if t[1].eval != 'error' and t[3].eval != 'error':
        t[0].eval = t[1].eval / t[3].eval
        t[0].value = Node('divide', [], t[0].eval)
    elif hasattr(t[1], 'name') and hasattr(t[3], 'name'):
        # The code below evaluates constant expressions like (a / a)
        if t[1].name == t[3].name and \
           not hasattr(t[1], 'isarray') and \
           not hasattr(t[3], 'isarray'):
            if t[1].datatype == ('int', None) and \
               t[3].datatype == ('int', None):
                t[0].eval = 1
                t[0].value = Node('int', [], t[0].eval)
            elif t[1].datatype == ('float', None) and \
                 t[3].datatype == ('float', None):
                t[0].eval = 1.0
                t[0].value = Node('float', [], t[0].eval)
        else:
            t[0].value = Node('divide', [t[1].value, t[3].value])
            t[0].eval = 'error'
    elif t[3].eval == 1:
        t[0].eval = 'error'
        t[0].value = t[1].value
        if hasattr(t[1], 'name'):
            t[0].name = t[1].name
    elif hasattr(t[3], 'name') and t[1].eval == 0:
        t[0].eval = 0
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    else:
        t[0].eval = 'error'
        t[0].value = Node('divide', [t[1].value, t[3].value])


def p_expression_uminus(t):
    'expression : MINUS expression %prec UMINUS'
    t[0].datatype = t[2].datatype
    if hasattr(t[2], 'noindex'):
        t[0].noindex = t[2].noindex
    t[0].lineno = t[2].lineno 
    if t[2].eval != 'error':
        t[0].eval = - t[2].eval
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    else:
        t[0].eval = 'error'
        t[0].value = Node('uminus', [t[2].value])


def p_expression_uplus(t):
    'expression : PLUS expression %prec UPLUS'
    t[0].datatype = t[2].datatype
    if hasattr(t[2], 'noindex'):
        t[0].noindex = t[2].noindex    
    t[0].lineno = t[2].lineno
    t[0].eval = t[2].eval
    if t[0].eval != 'error':
        t[0].value = Node(t[0].datatype[0], [], t[0].eval)
    else:
        t[0].value = t[2].value
    if hasattr(t[2], 'name'):
        t[0].name = t[2].name


def p_expression_funcall(t):
    'expression : ID LPAREN exprs RPAREN'
    t[0].datatype = flameerr.errtype
    t[0].value = Node('function_call', [t[3].value], t[1].value)
    t[0].lineno = t[4].lineno
    t[0].eval = 'error'
    # Check if the function is undeclared
    if isundecl(t[1]):
        flameerr.flerror(t[1].lineno, 51, (t[1].value))
        t[1].symtab.datatype = flameerr.errtype
        t[0].datatype = t[1].symtab.datatype
        return
    # Make sure ID is a function
    if not hasattr(t[1].symtab, 'isfunction'):
        flameerr.flerror(t[1].lineno, 59, (t[1].value))
        t[1].symtab.datatype = flameerr.errtype
        t[0].datatype = t[1].symtab.datatype
        return
    # Check no. arguments
    nr_args = t[1].symtab.argcount

    maxargs = symtab.get_attr('$maxargs', 'size')
    symtab.set_attr('$maxargs', 'size', max(nr_args, maxargs))

    if nr_args != t[3].len:
        flameerr.flerror(t[2].lineno, 29, (t[1].value))
    # Check argument types
    for i in range(t[3].len):
        if nr_args <= i:
            break
        if t[1].symtab.argtypes[i] != t[3].datatype[i]:
            # Type mismatch in function and call
            flameerr.flerror(t[1].lineno, 28, (i+1, t[1].value))
    t[0].function = t[1].value
    t[0].datatype = (t[1].symtab.datatype.type, None)
    # Keep track of where this function was used as an expression
    flameerr.addfunexpr(t[1].value, t[1].lineno)


def p_expression_location(t):
    'expression : location'
    t[0].value = t[1].value
    t[0].lineno = t[1].lineno
    try:
        t[0].datatype = t[1].datatype
        t[0].name = t[1].name
        t[0].eval = 'error'

    except AttributeError:
        return

    if hasattr(t[1], 'isarray'):
        t[0].isarry = 1
    if hasattr(t[1], 'noindex'):
        t[0].noindex = t[1].noindex


def p_expression_number_int(t):
    'expression : INUMBER'
    t[0].lineno = t[1].lineno
    t[0].datatype = ('int', None)
    t[0].eval = int(t[1].value)
    t[0].value = Node('int', [], t[0].eval)
    

def p_expression_number_float(t):
    'expression : FNUMBER'
    t[0].lineno = t[1].lineno
    t[0].datatype = ('float', None)
    t[0].eval = float(t[1].value)
    t[0].value = Node('float', [], t[0].eval)
    

def p_expression_int_cast(t):
    'expression : INT LPAREN expression RPAREN'
    t[0].lineno = t[4].lineno
    t[0].datatype = ('int', None)
    # Convert the evaluated expression
    # to type int, if possible
    if t[3].eval != 'error':
        t[0].eval = int(t[3].eval)
        t[0].value = Node('int', [], t[0].eval)
    else:
        t[0].eval = 'error'
        t[0].value = Node('int_cast', [t[3].value])
    if hasattr(t[3], 'name'):
        t[0].name = t[3].name
        

def p_expression_float_cast(t):
    'expression : FLOAT LPAREN expression RPAREN'
    t[0].lineno = t[4].lineno
    t[0].datatype = ('float', None)
    # Convert the evaluated expression
    # to type float, if possible
    if t[3].eval != 'error':
        t[0].eval = float(t[3].eval)
        t[0].value = Node('float', [], t[0].eval)
    else:
        t[0].eval = 'error'
        t[0].value = Node('float_cast', [t[3].value])
    if hasattr(t[3], 'name'):
        t[0].name = t[3].name


def p_expression_group(t):
    'expression : LPAREN expression RPAREN'
    t[0].value = t[2].value
    t[0].lineno = t[3].lineno
    t[0].datatype = t[2].datatype
    t[0].eval = t[2].eval
    if hasattr(t[2], 'name'):
        t[0].name = t[2].name


def p_type_int(t):
    'type : INT'
    t[0].value = Node('int', [])
    t[0].lineno = t[1].lineno
    t[0].datatype = ('int', None)


def p_type_float(t):
    'type : FLOAT'
    t[0].value = Node('float', [])
    t[0].lineno = t[1].lineno
    t[0].datatype = ('float', None)


def p_type_int_array(t):
    'type : INT array'
    t[0].value = Node('int', [t[2].value])
    t[0].lineno = t[2].lineno
    if t[2].eval == 'error':
        # If the expression was not evaluated
        # The type is then set to the error type
        t[0].datatype = flameerr.errtype
    else:
        # Check the type of the array dimension
        if t[2].datatype[0] != 'int':
            # Type should be an int
            t[0].datatype = flameerr.errtype
        else:
            # Check for index <= 0
            if t[2].eval <= 0:
                flameerr.flerror(t[2].lineno, 31, ())
            t[0].datatype = ('int', t[2].eval)


def p_type_float_array(t):
    'type : FLOAT array'
    t[0].value = Node('float', [t[2].value])
    t[0].lineno = t[2].lineno
    if t[2].eval == 'error':
        # If the expression was not evaluated
        # The type is then set to the error type
        t[0].datatype = flameerr.errtype
    else:
        # Check the type of the array dimension
        if t[2].datatype[0] != 'int':
            # Type should be an int
            t[0].datatype = flameerr.errtype
        else:
            # Check for index <= 0
            if t[2].eval <= 0:
                flameerr.flerror(t[2].lineno, 31, ())
            t[0].datatype = ('float', t[2].eval)


def p_array(t):
    'array : LBRACE expression RBRACE'
    t[0].value = Node('array_size', [t[2].value])
    t[0].lineno = t[3].lineno
    t[0].eval = t[2].eval
    t[0].datatype = t[2].datatype


def p_array_empty_dim_error(t):
    'array : LBRACE RBRACE'
    # Empty array dimension
    flameerr.flerror(t[2].lineno, 24)
    t[0].value = Node('error', [])
    t[0].lineno = t[2].lineno


def p_array_dim_error(t):
    'array : LBRACE error RBRACE'
    # Bad expression in array dimension
    flameerr.flerror(t[2].lineno, 25)
    t[0].value = Node('error', [])
    t[0].lineno = t[3].lineno
    

def p_literal_string(t):
    'literal : strings'
    t[0].value = Node('string', [], concat(t[1].value))


# The few p_str* rules below do not actually add any Nodes to the tree.
# They generate intermediate nodes, which are discarded above by the concat()
# function call in p_literal_string().
def p_strings_string(t):
    'strings : STRING'
    t[0].value = Node(None, [], t[1].value)


def p_strings(t):
    'strings : strings str'
    t[0].value = t[1].value
    t[0].value.children.append(t[2].value)


def p_str(t):
    'str : STRING'
    t[0].value = Node('string', [], t[1].value)


def p_empty(t):
    'empty :'
    pass

def p_error(t):
    if t:
        print "Line %d. Unexpected token %s" % (t.lineno,t.value)
    else:
        print "Syntax error. EOF"

        
# Build the parse table
pyson.yacc()


# This utility function prints out a nice looking parse tree.
def dump_tree(n,indent=""):
    if n.value == None:
        print "%s%s" % (indent, n.type)
    else:
        print "%s%s (%s)" % (indent, n.type, n.value)
    indent = indent.replace("-"," ")
    indent = indent.replace("+"," ")
    for i in range(len(n.children)):
        c = n.children[i]
        if i == len(n.children)-1:
            dump_tree(c,indent + "  +-- ")
        else:
            dump_tree(c,indent + "  |-- ")

def parse(data):
    return pyson.parse(data)
                
# A special program to run if the user runs the parser as a main
# program.  For instance:
#
#   python calcparse.py

# Whether we're printing the tree or not
print_tree = None

if __name__ == '__main__':
    import sys
    try:
        file = sys.argv[1]
        f = open(file)
        data = f.read()
        f.close();
        if len(sys.argv) > 2:
            print_tree = 1
    except IndexError:
        # Hmmm. No file specified. Just read from stdin
        print "Type some text followed by an EOF (Ctrl-D)"
        data = sys.stdin.read()
        print_tree = 1
        # Parse the data.  This will start calling the action
        # functions above.  The returned value is the value of
        # t[0].value as set in the very first grammar rule.
        # (in this case, we hope it is the top of the AST).
    
    # profile.run('a = pyson.parse(data)')
    a = pyson.parse(data)

    # Dump out the tree (if we got any return value from parse()) and
    # if we passed more than two arguments to Python
    if a and print_tree:
        # Please do not remove this line if you include this code in your
        # solution
         print ":::: Parse Tree ::::"
         dump_tree(a)

