# flameerr.py
# Flame error module. This module keeps error messages from
# littering up the rest of the code.
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: flameerr.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $

# Import the random number module
import whrandom

# Define an error type
errtype = ("error", None)

# Set to true if any error occurs
nocompile = 0

# Define a bunch of error messages with format specifiers
errmsg = (
    "Floating point literal in array dimension",        # 0
    "Non-constant in array dimension",                  # 1
    "Division by zero in expression",                   # 2
    "Array dimension <= 0",                             # 3
    "Array index is not an integer",                    # 4
    "Cannot mix types %s and %s",                       # 5
    "Function %s is redeclared",                        # 6
    "Variable %s is redeclared",                        # 7
    "Function %s is undeclared",                        # 8
    "Variable %s is undeclared",                        # 9
    "Empty begin/end block",                            # 10
    "Misplaced declaration",                            # 11
    "Bad relational expression in while",               # 12
    "Bad relational expression in if",                  # 13
    "Misplaced else",                                   # 14
    "Bad expression in assignment",                     # 15
    "Expected a string in print()",                     # 16
    "Bad expression in write()",                        # 17
    "Bad location in read()",                           # 18
    "Bad expression in return",                         # 19
    "Unindexed array '%s'",                             # 20
    "Indexed non-array variable '%s'",                  # 21
    "Bad expression in array index of %s",              # 22
    "Missing ',' in expression list",                   # 23
    "Empty array dimension",                            # 24
    "Bad expression in array dimension",                # 25
    "Wrong number of arguments to function %s",         # 26
    "Array dimension is not an integer",                # 27
    "Type error in argument %d of call to function %s", # 28
    "Wrong number of arguments to function %s",         # 29
    "Type error in assignment",                         # 30
    "Array index out of bounds",                        # 31
    "Array index of %s must be an integer",             # 32
    "%s is not an array",                               # 33
    "Misplaced break",                                  # 34
    "Bad type in read()",                               # 35
    "Bad type in write()",                              # 36
    "Bad return type",                                  # 37
    "Type error in binary operation +",                 # 38
    "Type error in binary operation *",                 # 39
    "Type error in binary operation /",                 # 40
    "Type error in binary operation -",                 # 41
    "Type error in binary operation !=",                # 42
    "Type error in binary operation ==",                # 43
    "Type error in binary operation <",                 # 44
    "Type error in binary operation >",                 # 45
    "Type error in binary operation <=",                # 46
    "Type error in binary operation >=",                # 47
    "Conflicting return type for function",             # 48
    "Redeclared identifier %s",                         # 49
    "Undeclared identifier %s",                         # 50
    "Function %s is undeclared",                        # 51
    "No return type could be determined for " + \
    "function %s",                                      # 52
    "Control might reach end of non-void function" + \
    " %s without a return",                             # 53
    "Function %s returns no value but is used in " + \
    "an expression on line %i",                         # 54
    "Bad constant expression",                          # 55
    "Function %s redeclared",                           # 56
    "Missing ';'",                                      # 57
    "Bad typename '%s'",                                # 58
    "%s is not a function",                             # 59
    "main is not defined",                              # 60
    )


# Words of encouragement
insults = (
    "It looks like you need to go back to coding LOGO",
    "Some kindergarteners called. They want their code back",
    "Apparently you haven't learned from your last dumb mistake",
    "I see you've discovered how to screw up in O(1) time",
    "Your program needs to be ported to the trash",
    "This code has the Y2K bug",
    "Maybe you should just stick to batch files",
    "Even the perl interpreter wouldn't accept this code",
    "The garbage collector is collecting your whole program",
    "You seem to be in a race with the Windows team for most bugs",
    )

# I'm too lazy to count
insultslen = len(insults)

# error number
msgno = 0

# Maintain a copy of what was printed last, so that we
# don't print the same thing over and over
lastprint = None

# Maintain an option variable to decide if we want to
# keep track of the last print statement or not
flerror_quiet = 0

# A function similar to perror(strerror(...)) in C.
# Prints the specified line number and error message.
# Note for future: maybe errno should be global (like in C)
def flerror(lineno, errno, args=()):
    global lastprint
    global msgno
    global nocompile
    nocompile = 1
    if flerror_quiet:
        if lastprint == (lineno, errno, args):
            # Don't print anything
            return
        lastprint = (lineno, errno, args)
    print "Line %i." % lineno,
    print errmsg[errno] % args
    msgno += 1
    # Give some loving advice every so many errors
    if msgno % int(insultslen / 2) == 0:
        print insults[whrandom.randint(0, insultslen-1)]


# Keep track of where functions were used as expressions
# (this helps for error no. 54)
# Each dictionary key is the function name string
# and each value is a list of line numbers.
funexprs = {}


# Keep track of the line numbers functions were declared on
# (this is probably redundant, but it's easier -- maybe I should
#  do it the right way later)
funlines = {}


# A function to make it easy to add an instance of
# a function being used as an expression
def addfunexpr(name, line):
    global funexprs
    # print "addfunexpr(%s, %i) called." % (name, line)
    if funexprs.has_key(name):
        funexprs[name].append(line)
    else:
        funexprs[name] = [line]

