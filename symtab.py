# symtab.py
# Flame compiler symbol table module
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: symtab.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $	


# Class for storing information about a symbol in a table.
# We can always add new attributes to instances of this class
class Symbol:
    def __init__(self):
        self.datatype = None


# The table stack, initialized with the global symbol table
tables = [{}]


# The stack top
top = 0


# The maximum nesting level
# (maintained by get_nestlev())
maxnest = 0


# A function to add a new symbol table
def push_table():
    global top
    tables.append({})
    top += 1


# A function to remove a table and restore the previous
def pop_table():
    global top
    if len(tables) > 1:
        t = tables.pop()
        top -= 1
        return t
    else:
        return None


# A function to add a symbol to the current table
def add_new_symbol(sym):
    global top
    tables[top][sym] = Symbol()
    return tables[top][sym]


# Add a symbol that already exists
def add_symbol(sym, entry):
    global top
    tables[top][sym] = entry
    return entry


# A function to look up a symbol in the current table
def local_lookup(sym):
    global top
    return tables[top].get(sym, None)


# Do a lookup in a specific table
def lookup_in(sym, table):
    return table.get(sym, None)


# A function to look up a symbol in all tables outside the local one
def global_lookup(sym):
    global top
    i = top - 1
    found = None

    # Try to look up sym, moving outward in scope each time
    while not found and i >= 0:
        found = tables[i].get(sym, None)
        i -= 1

    # If we found it and the nesting level hasn't been set, set it
    if found and not hasattr(found, 'nestlev'):
        found.nestlev = i + 1

    return found


# A function to set a symbol attribute
def set_attr(name, attr, value):
    sym = local_lookup(name)
    assert(sym != None)
    setattr(sym, attr, value)


# A function to get an attribute
def get_attr(name, attr):
    sym = local_lookup(name)
    assert(sym != None)
    return getattr(sym, attr)


# Get the current nesting level
def get_nestlev():
    global top
    global maxnest
    if top - 1 > maxnest:
        maxnest = top - 1
    return top - 1


# A debugging function
def dump():
    global top
    return tables[top].keys()

def dump_all():
    t = top
    while t >= 0:
        print tables[t].items()
        print "\n"
        t -= 1
        






