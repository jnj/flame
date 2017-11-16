# flametype.py
# Type inference manager module
# This module aids in managing the types of functions.
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: flametype.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $

# Use the error module
import flameerr


# Define a class for function return type
class Type:
    def __init__(self):
        self.type = None


# An object to hold the Type objects
types = {}


# Keep track of the current function during the parse
fstack = []
fstacksize = 0
fstacktop = -1


# Functions to manage the current function
# This is not as easy as it seems. Nested functions
# make this a bit tricky.
def openfunction(name):
    global fstack
    global fstacktop
    global fstacksize
    fstack.append(name)
    fstacksize += 1
    fstacktop += 1
    

def closefunction():
    global fstack
    global fstacktop
    global fstacksize
    fstack.pop()
    fstacksize -= 1
    fstacktop -= 1

    
def getfunction():
    global fstacktop
    # print "Current function is %s" % fstack[fstacktop]
    return fstack[fstacktop]


# A function to create a new unknown Type
def newtype(name):
    global types
    t = Type()
    types[name] = t
    return t


# A function to get the Type of a function
def gettype(name):
    global types
    if types.has_key(name):
        return types[name]
    else:
        return newtype(name)


# A function to set the Type of a function
def settype(name, type):
    global types
    # print "settype(%s, %s) called." % (name, type)
    types[name].type = type
    return types[name]


# A function to specify that two functions return the same type
def unify(a, b):
    global types

    # The inference logic in the parser will avoid situations
    # where we try to unify two different types. In here we can
    # assume they are the same.
    u = gettype(a)
    v = gettype(b)

    # See if we already know one of the types
    # If so, set the other type to it
    if u.type:
        types[b] = types[a]
    else:
        types[a] = types[b]


# Report any instances of void functions being used as expressions
def reportvoids():
    global types
    def printer(pair):
        if pair[1].type == 'void':
            for i in flameerr.funexprs.get(pair[0], []):
                flameerr.flerror(flameerr.funlines[pair[0]], 54, (pair[0], i))
    map(printer, types.items())

# Report functions for which no type could be determined
def reportnones():
    global types
    def printer(pair):
        if pair[1].type == None:
            for i in flameerr.funexprs.get(pair[0], []):
                flameerr.flerror(flameerr.funlines[pair[0]], 52, (pair[0]))
    map(printer, types.items())

# Debugging function
def dump():
    global types
    def printer(pair):
        print "%s() returns type %s" % (pair[0], pair[1].type)
    map(printer, types.items())
    
    
