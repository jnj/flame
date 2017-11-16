#-----------------------------------------------------------------------------
# pyson.py
#
# Author: David M. Beazley (beazley@cs.uchicago.edu)
#         Department of Computer Science
#         University of Chicago
#
# Copyright (C) 2001, University of Chicago
#
# $Header: /cvs/projects/PyParse/pyson/pyson.py,v 1.34 2001/05/09 17:51:08 beazley Exp $
#
# This implements an LR parser that is constructed from grammar rules defined
# as Python functions.  Roughly speaking, this module is a cross between
# John Aycock's Spark system and the GNU bison utility.
#
# Disclaimer:  This is a work in progress. SLR parsing seems to work fairly
# well and there is extensive error checking. LR(1) parsing is implemented,
# but table generation is extremely slow. LALR(1) parsing is being worked on.
#-----------------------------------------------------------------------------

__version__ = "0.1"

import re, types, sys, cStringIO, md5, os.path
import plex
    
debug      = 1                 # Debugging.  If this is on, a 'parser.out' file
                               # is generated in the current directory.

debug_file  = 'parser.out'     # Name of the debugging file
tab_module  = 'parsetab'       # Parsing table module name
default_lr  = 'SLR'            # Default table generation method
get_token   = plex.token       # Default tokenizer

reduce_func = None             # This function, if supplied, is used to make
                               # reductions.  It gets the arguments (n,func,t)
                               # where n is the production number, func is the
                               # function, and t is the stack items

#maxstack    = 500


# Exception raised if an unrecoverable error occurs in building the grammar
class PysonError(Exception):   pass

# This class is used to hold derived symbols during parsing. It normally
# has the same attributes as tokens.

class PysonSymbol:
    def __str__(self):    return self.type
    def __repr__(self):   return str(self)


# -----------------------------------------------------------------------------
#                           === FILE VALIDATION ===
#
# This checks to see if there are duplicated p_rulename() functions in the
# parser input file.  This is done using a simple regular expression
# match on each line in filename.
# -----------------------------------------------------------------------------

def validate_file(filename):
    base,ext = os.path.splitext(filename)
    if ext != '.py': return 1          # No idea. Assume it's okay.

    try:
        f = open(filename)
        lines = f.readlines()
        f.close()
    except IOError:
        return 1                       # Oh well

    fre = re.compile(r'\s*def\s+(p_[a-zA-Z_0-9]*)\(')
    counthash = { }
    linen = 1
    for l in lines:
        m = fre.match(l)
        if m:
            name = m.group(1)
            prev = counthash.get(name)
            if not prev:
                counthash[name] = linen
            else:
                print "%s:%d: Function %s redefined. Previously defined on line %d" % (filename,linen,name,prev)
        linen += 1

# -----------------------------------------------------------------------------
#                           === GRAMMAR FUNCTIONS ===
#
# The following global variables and functions are used to store, manipulate,
# and verify the grammar rules specified by the user.
# -----------------------------------------------------------------------------

def initialize_vars():
    global Productions, Prodnames, Prodmap, Prodempty, Terminals 
    global Nonterminals, First, Follow, Precedence, LRitems
    global Errorfunc, Signature, Requires


    Productions  = [None]  # A list of all of the productions.  The first
                           # entry is always reserved for the purpose of
                           # building an augmented grammar
                        
    Prodnames    = { }     # A dictionary mapping the names of nonterminals to a list of all
                           # productions of that nonterminal.
                        
    Prodmap      = { }     # A dictionary that is only used to detect duplicate
                           # productions.

    Prodempty    = { }     # A dictionary of all productions that have an empty rule
                           # of the form P : <empty>

    Terminals    = { }     # A dictionary mapping the names of terminal symbols to a
                           # list of the rules where they are used.

    Nonterminals = { }     # A dictionary mapping names of nonterminals to a list
                           # of rule numbers where they are used.

    First        = { }     # A dictionary of precomputed FIRST(x) symbols
    
    Follow       = { }     # A dictionary of precomputed FOLLOW(x) symbols

    Precedence   = { }     # Precedence rules for each terminal. Contains tuples of the
                           # form ('right',level) or ('left',level)

    LRitems      = [ ]     # A list of all LR items for the grammar.  These are the
                           # productions with the "dot" like E -> E . PLUS E

    Errorfunc    = None    # User defined error handler

    Signature    = md5.new()   # Digital signature of the grammar rules, precedence
                               # and other information.  Used to determined when a
                               # parsing table should be regenerated.

    Requires     = { }     # Requires list
    
    
# Verbose file
_vf           = cStringIO.StringIO()
_vfc          = cStringIO.StringIO()

# -----------------------------------------------------------------------------
# class Production:
#
# This class stores the raw information about a single production or grammar rule.
# It has a few required attributes:
#
#       name     - Name of the production (nonterminal)
#       prod     - A list of symbols making up its production
#       number   - Production number.
#
# In addition, a few additional attributes are used to help with debugging or
# optimization of table generation.
#
#       file     - File where production action is defined.
#       lineno   - Line number where action is defined
#       func     - Action function
#       prec     - Precedence left
#       lr_next  - Next LR item. Example, if we are ' E -> E . PLUS E'
#                  then lr_next refers to 'E -> E PLUS . E'   
#       lr_index - LR item index (location of the ".") in the prod list.
#       len      - Length of the production (number of symbols on right)
# -----------------------------------------------------------------------------

class Production:
    def __init__(self,**kw):
        for k,v in kw.items():
            setattr(self,k,v)
        self.lr_index = -1
        self.lr0_added = 0       # Flag indicating whether or not added to LR0 closure
        self.usyms = [ ]
        
    def __str__(self):
        if self.prod:
            s = "%s -> %s" % (self.name," ".join(self.prod))
        else:
            s = "%s -> <empty>" % self.name
        return s

    def __repr__(self):
        return str(self)

    # Compute lr_items from the production
    def lr_item(self,n):
        if n > len(self.prod): return None
        p = Production()
        p.name = self.name
        p.prod = list(self.prod)
        p.number = self.number
        p.lr_index = n
        p.prod.insert(n,".")
        p.prod = tuple(p.prod)
        p.len = len(p.prod)
        p.usyms = self.usyms

        # Precompute list of productions immediately following
        try:
            p.lrafter = Prodnames[p.prod[n+1]]
        except (IndexError,KeyError),e:
            p.lrafter = []
        try:
            p.lrbefore = p.prod[n-1]
        except IndexError:
            p.lrbefore = None

        return p
            
# -----------------------------------------------------------------------------
# add_production()
#
# Given an action function, this function assembles a production rule.
# The production rule is assumed to be found in the function's docstring.
# This rule has the general syntax:
#
#              name1 ::= production1
#                     |  production2
#                     |  production3
#                    ...
#                     |  productionn
#              name2 ::= production1
#                     |  production2
#                    ... 
# -----------------------------------------------------------------------------

def add_production(f,file,line,prodname,syms):
    if Terminals.has_key(prodname):
        print "%s:%d. Illegal rule name '%s'. Already defined as a token." % (file,line,prodname)
        return -1
    if prodname == 'error':
        print "%s:%d. Illegal rule name '%s'. error is a reserved word." % (file,line,prodname)
        return -1
                
    if not is_identifier(prodname):
        print "%s:%d. Illegal rule name '%s'" % (file,line,prodname)
        return -1

    for s in syms:
        if not is_identifier(s) and s != '%prec':
            print "%s:%d. Illegal name '%s' in rule '%s'" % (file,line,s, prodname)
            return -1

    # See if the rule is already in the rulemap
    map = "%s -> %s" % (prodname,syms)
    if Prodmap.has_key(map):
        m = Prodmap[map]
        print "%s:%d. Duplicate rule %s." % (file,line, m)
        print "%s:%d. Previous definition at %s:%d" % (file,line, m.file, m.line)
        return -1

    p = Production()
    p.name = prodname
    p.prod = syms
    p.file = file
    p.line = line
    p.func = f
    p.number = len(Productions)

            
    Productions.append(p)
    Prodmap[map] = p
    if not Nonterminals.has_key(prodname):
        Nonterminals[prodname] = [ ]
    
    # Add all terminals to _terminal rule
    i = 0
    while i < len(p.prod):
        t = p.prod[i]
        if t == '%prec':
            try:
                precname = p.prod[i+1]
            except IndexError:
                print "%s:%d. Syntax error. Nothing follows %%prec." % (p.file,p.line)
                return -1

            prec = Precedence.get(precname,None)
            if not prec:
                print "%s:%d. Nothing known about the precedence of '%s'" % (p.file,p.line,precname)
                return -1
            else:
                p.prec = prec
            del p.prod[i]
            del p.prod[i]
            continue

        if Terminals.has_key(t):
            Terminals[t].append(p.number)
            # Is a terminal.  We'll assign a precedence to p based on this
            if not hasattr(p,"prec"):
                p.prec = Precedence.get(t,('right',0))
        else:
            if not Nonterminals.has_key(t):
                Nonterminals[t] = [ ]
            Nonterminals[t].append(p.number)
        i += 1

    if not hasattr(p,"prec"):
        p.prec = ('right',0)
        
    # Set final length of productions
    p.len  = len(p.prod)
    p.prod = tuple(p.prod)

    # Calculate unique syms in the production
    p.usyms = [ ]
    for s in p.prod:
        if s not in p.usyms:
            p.usyms.append(s)
    
    # Add to the global productions list
    try:
        Prodnames[p.name].append(p)
    except KeyError:
        Prodnames[p.name] = [ p ]
    return 0

# Given a raw rule function, this function rips out its doc string
# and adds rules to the grammar

def add_function(f):
    line = f.func_code.co_firstlineno
    file = f.func_code.co_filename
    error = 0
    
    if f.func_code.co_argcount > 1:
        print "%s:%d. Rule '%s' has too many arguments." % (file,line,f.__name__)
        return -1

    if f.func_code.co_argcount < 1:
        print "%s:%d. Rule '%s' requires an argument." % (file,line,f.__name__)
        return -1
          
    if f.__doc__:
        # Split the doc string into lines
        pstrings = f.__doc__.splitlines()
        lastp = None
        dline = line
        for ps in pstrings:
            dline += 1
            p = ps.split()
            if not p: continue
            try:
                if p[0] == '|':
                    # This is a continuation of a previous rule
                    if not lastp:
                        print "%s:%d. Misplaced '|'." % (file,dline)
                        return -1
                    prodname = lastp
                    if len(p) > 1:
                        syms = p[1:]
                    else:
                        syms = [ ]
                else:
                    prodname = p[0]
                    lastp = prodname
                    assign = p[1]
                    if len(p) > 2:
                        syms = p[2:]
                    else:
                        syms = [ ]
                    if assign != ':' and assign != '::=':
                        print "%s:%d. Syntax error. Expected ':'" % (file,dline)
                        return -1
                e = add_production(f,file,dline,prodname,syms)
                error += e
            except StandardError:
                print "%s:%d. Syntax error in rule '%s'" % (file,dline,ps)
                error -= 1
    return error

# -----------------------------------------------------------------------------
# check_cycles()
#
# This function looks at the various parsing rules and tries to detect
# infinite recursion cycles (grammar rules where there is no possible way
# to derive a string of terminals).
# -----------------------------------------------------------------------------

def check_cycles(p=None,val=1):
    if not p:
        # Walk through list of productions and zero out cycle counts
        for i in Productions:
            i.cychk = 0            # Already encountered this rule
            i.cyvalue = -1
        error = 0

        # First run a cycle check on the top rule.  This will find
        # everything that's used.
        
        p = Productions[1]
        c = check_cycles(p,1)
        val = 2
        for n,pl in Prodnames.items():
            term = 0
            used = 0
            for p in pl:
                if not p.cychk:
                    continue
                used +=1
                term += check_cycles(p,val)
            if not used:
                print "pyson: Rule '%s' never used." % n
                
            if used and not term and Nonterminals[n]:
                print "pyson: Infinite recursion detected in rule '%s'." % n
                error = 1
            val += 1
        return error

    # We are checking this rule for a cycle
    if p.cychk == val:
        # Hmmm. We already saw this rule before.
        if p.cyvalue >= 0:
            return p.cyvalue
        else:
            return 0

    p.cyvalue = -1
    # Looking for cycles
    p.cychk = val

    # Look at all of the symbols to the right of this production
    term = 0
    for s in p.prod:
        # Get list of productions for 's'
        pl = Prodnames.get(s,None)
        if not pl:
            term += 1
            continue           # Must be a terminal. Continue
        
        # Check all of the rules for each non-terminal for cycles
        pterm = 0
        for x in pl:
            c = check_cycles(x,val)
            if c:
                pterm = 1
                if val > 1: break
        term += pterm

    # All right hand side symbols terminate
    # print p.name, term, len(p.prod), val
    if term == len(p.prod):
        p.cyvalue = 1
    else:
        p.cyvalue = 0
        
    return p.cyvalue

# -----------------------------------------------------------------------------
# verify_productions()
#
# This function examines all of the supplied rules to see if they seem valid.
# -----------------------------------------------------------------------------

def verify_productions():
    error = 0
    for p in Productions:
        if not p: continue

        # Look for a purely empty productions and record in special dictionary
        if not len(p.prod):
            Prodempty[p.name] = p
            continue
        
        for s in p.prod:
            if not Prodnames.has_key(s) and not Terminals.has_key(s) and s != 'error':
                print "%s:%d. Symbol '%s' used, but not defined as a token or a rule." % (p.file,p.line,s)
                error = 1
                continue

    unused_tok = 0 
    # Now verify all of the tokens
    if debug:
        _vf.write("Unused terminals:\n\n")
    for s,v in Terminals.items():
        if s != 'error' and not v:
            print "pyson: Warning. Token '%s' defined, but not used." % s
            if debug: _vf.write("   %s\n"% s)
            unused_tok += 1

    # Print out all of the productions
    if debug:
        _vf.write("\nGrammar\n\n")
        for i in range(1,len(Productions)):
            _vf.write("Rule %-5d %s\n" % (i, Productions[i]))
        
    unused_prod = 0
    # Verify the use of all productions
    for s,v in Nonterminals.items():
        if not v:
            p = Prodnames[s][0]
            print "%s:%d: Warning. Rule '%s' defined, but not used." % (p.file,p.line, s)
            unused_prod += 1

    
    if unused_tok == 1:
        print "pyson: Warning. There is 1 unused token."
    if unused_tok > 1:
        print "pyson: Warning. There are %d unused tokens." % unused_tok

    if unused_prod == 1:
        print "pyson: Warning. There is 1 unused rule."
    if unused_prod > 1:
        print "pyson: Warning. There are %d unused rules." % unused_prod

    if debug:
        _vf.write("\nTerminals, with rules where they appear\n\n")
        for k in Terminals.keys():
            _vf.write("%-20s : %s\n" % (k, " ".join([str(s) for s in Terminals[k]])))
        _vf.write("\nNonterminals, with rules where they appear\n\n")
        for k in Nonterminals.keys():
            _vf.write("%-20s : %s\n" % (k, " ".join([str(s) for s in Nonterminals[k]])))

    error += check_cycles()
    return error

# -----------------------------------------------------------------------------
# build_lritems()
#
# This function walks the list of productions and builds a complete set of the
# LR items.  The LR items are stored in two ways:  First, they are uniquely
# numbered and placed in the list _lritems.  Second, a linked list of LR items
# is built for each production.  For example:
#
#   E -> E PLUS E
#
# Creates the list
#
#  [E -> . E PLUS E, E -> E . PLUS E, E -> E PLUS . E, E -> E PLUS E . ] 
# -----------------------------------------------------------------------------

def build_lritems():
    for p in Productions:
        lastlri = p
        lri = p.lr_item(0)
        i = 0
        while 1:
            lri = p.lr_item(i)
            lastlri.lr_next = lri
            if not lri: break
            lri.lr_num = len(LRitems)
            LRitems.append(lri)
            lastlri = lri
            i += 1

    # In order for the rest of the parser generator to work, we need to
    # guarantee that no more lritems are generated.  Therefore, we nuke
    # the p.lr_item method

    Production.lr_item = None

# -----------------------------------------------------------------------------
# add_precedence()
#
# Given a list of precedence rules, add to the precedence table.
# -----------------------------------------------------------------------------

def add_precedence(plist):
    plevel = 0
    error = 0
    for p in plist:
        plevel += 1
        try:
            prec = p[0]
            terms = p[1:]
            if prec != 'left' and prec != 'right':
                print "pyson: Invalid precedence '%s'" % prec
                return -1
            for t in terms:
                if Precedence.has_key(t):
                    print "pyson: Precedence already specified for terminal '%s'" % t
                    error += 1
                    continue
                Precedence[t] = (prec,plevel)
        except:
            print "pyson: Invalid precedence table.\n"
            error += 1

    return error

# -----------------------------------------------------------------------------
# augment_grammar()
#
# Compute the augmented grammar.  This is just a rule S' -> start where start
# is the starting symbol.
# -----------------------------------------------------------------------------

def augment_grammar():
    Productions[0] = Production(name="S'",prod=[Productions[1].name],number=0,len=1,prec=('right',0),func=None)
    Productions[0].usyms = [ Productions[1].name ]
    Nonterminals[Productions[1].name].append(0)
    
# -----------------------------------------------------------------------------
# first()
#
# Compute the value of FIRST1(X) where x is a terminal, nonterminal, or tuple
# of grammar symbols.
# -----------------------------------------------------------------------------

def first(x):
    # Check if already computed
    fst = First.get(x,None)
    if fst: return fst
    
    if isinstance(x,types.TupleType):
        # We are computing First(x1,x2,x3,x4,...xn)
        fst = [ ]
        numempty = 0
        for s in x:
            f = first(s)
            for i in f:
                if i == '<empty>':
                    numempty +=1
                    continue
                if i not in fst: fst.append(i)
            if not numempty: break
        if numempty == len(x):
            fst.append('<empty>')

    elif Terminals.has_key(x):
        fst = [x]

    elif Prodnames.has_key(x):
        fst = [ ]
        prodlist = Prodnames[x]
        for p in prodlist:
            # Check if we're already computing first on this production
            if p.cfirst: continue
        
            # Check for empty, we just add empty to first in this case
            if not len(p.prod):
                fst.append('<empty>')
                p.cfirst = 1
                continue

            # Go down the list of production symbols looking for empty
            # productions

            p.cfirst = 1
            for ps in p.prod:
                if ps == '<empty>': continue
                # Add everything in first(ps) to our set

                f = first(ps)
                for i in f:
                    if i not in fst: fst.append(i)
                # If this rule doesn't produce empty, we're done
                if not Prodempty.has_key(ps):
                    break
    else:
        raise PysonError, "first: %s not a terminal or nonterminal" % x

    First[x] = fst
    return fst

# FOLLOW(x)
# Given a non-terminal.  This function computes the set of all symbols
# that might follow it.  Dragon book, p. 189.

def compute_follow():
    # Add '$' to the follow list of the start symbol
    for k in Nonterminals.keys():
        Follow[k] = [ ]

    start = Productions[1].name
    Follow[start] = [ '$' ]
        
    while 1:
        didadd = 0
        for p in Productions[1:]:
            # Here is the production set
            for i in range(len(p.prod)):
                B = p.prod[i]
                if Nonterminals.has_key(B):
                    # Okay. We got a non-terminal in a production
                    fst = first(p.prod[i+1:])
                    hasempty = 0
                    for f in fst:
                        if f != '<empty>' and f not in Follow[B]:
                            Follow[B].append(f)
                            didadd = 1
                        if f == '<empty>':
                            hasempty = 1
                    if hasempty or i == (len(p.prod)-1):
                        # Add elements of follow(a) to follow(b)
                        for f in Follow[p.name]:
                            if f not in Follow[B]:
                                Follow[B].append(f)
                                didadd = 1
        if not didadd: break

# Compute first for all non-terminals and such
def compute_first1():

    # Result first flag
    for p in Productions:
        p.cfirst = 0
        
    # Compute all terminals
    for t in Terminals.keys():
        first(t)

    # Compute for all nonterminals
    for n in Nonterminals.keys():
        prodlist = Prodnames[n]
        first(n)

    # First for the end symbol marker
    First['$'] = ['$']
    First['#'] = ['#']

# -----------------------------------------------------------------------------
#                           === SLR Generation ===
#
# The following functions are used to construct SLR (Simple LR) parsing tables
# as described on p.221-229 of the dragon book.
# -----------------------------------------------------------------------------

# Global variables for the LR parsing engine
def lr_init_vars():
    global _lr_action, _lr_goto, _lr_method
    global _lr_goto_cache
    
    _lr_action       = { }        # Action table
    _lr_goto         = { }        # Goto table
    _lr_method       = "Unknown"  # LR method used
    _lr_goto_cache   = { }

# Compute the LR(0) closure operation on I, where I is a set of LR(0) items.
# prodlist is a list of productions.

_add_count = 0       # Counter used to detect cycles
_empty = ()          # Preconstructed empty list to speed up iteration

def lr0_closure(I):
    global _add_count
    
    _add_count += 1
    prodlist = Productions
    
    # Add everything in I to J        
    J = I[:]
    didadd = 1
    while didadd:
        didadd = 0
        for j in J:
            for x in j.lrafter:
                if x.lr0_added == _add_count: continue
                # Add B --> .G to J
                J.append(x.lr_next)
                x.lr0_added = _add_count
                didadd = 1
               
    return J

# Compute the LR(0) goto function goto(I,X) where I is a set
# of LR(0) items and X is a grammar symbol.   This function is written
# in a way that guarantees uniqueness of the generated goto sets
# (i.e. the same goto set will never be returned as two different Python
# objects).   

def lr0_goto(I,x):
    # First we look for a previously cached entry
    g = _lr_goto_cache.get((id(I),x),None)
    if g: return g

    # Now we generate the goto set in a way that guarantees uniqueness
    # of the result
    
    s = _lr_goto_cache.get(x,None)
    if not s:
        s = { }
        _lr_goto_cache[x] = s

    gs = [ ]
    for p in I:
        n = p.lr_next
        if n and n.lrbefore == x:
            s1 = s.get(id(n),None)
            if not s1:
                s1 = { }
                s[id(n)] = s1
            gs.append(n)
            s = s1
    g = s.get('$',None)
    if not g:
        if gs:
            g = lr0_closure(gs)
            s['$'] = g
        else:
            s['$'] = gs
    _lr_goto_cache[(id(I),x)] = g
    return g

# Compute the kernel of a set of LR(0) items

def lr0_kernel(I):
    KI = [ ]
    for p in I:
        if p.name == "S'" or p.lr_index > 0 or p.len == 0:
            KI.append(p)

    return KI

_lr0_cidhash = { }

# Compute the LR(0) sets of item function
def lr0_items():
    
    C = [ lr0_closure([Productions[0].lr_next]) ]
    i = 0
    for I in C:
        _lr0_cidhash[id(I)] = i
        i += 1

    # Loop over the items in C and each grammar symbols
    i = 0
    while i < len(C):
        I = C[i]
        i += 1

        # Collect all of the symbols that could possibly be in the goto(I,X) sets
        asyms = { }
        for ii in I:
            for s in ii.usyms:
                asyms[s] = None

        for x in asyms.keys():
            g = lr0_goto(I,x)
            if not g:  continue
            if _lr0_cidhash.has_key(id(g)): continue
            _lr0_cidhash[id(g)] = len(C)            
            C.append(g)
            
    return C

# -----------------------------------------------------------------------------
# slr_parse_table()
#
# This function constructs an SLR table.
# -----------------------------------------------------------------------------
def slr_parse_table():
    global _lr_method
    goto = _lr_goto           # Goto array
    action = _lr_action       # Action array
    actionp = { }             # Action production array (temporary)

    _lr_method = "SLR"
    
    n_srconflict = 0
    n_rrconflict = 0

    print "pyson: Generating SLR parsing table..."
    if debug:
        _vf.write("\n\nParsing method: SLR\n\n")
        
    # Step 1: Construct C = { I0, I1, ... IN}, collection of LR(0) items
    # This determines the number of states
    
    C = lr0_items()

    # Build the parser table, state by state
    st = 0
    for I in C:
        # Loop over each production in I
        actlist = [ ]              # List of actions
        
        if debug:
            _vf.write("\nstate %d\n\n" % st)
            for p in I:
                _vf.write("    %s\n" % str(p))
            _vf.write("\n")

        for p in I:
            try:
                if p.prod[-1] == ".":
                    if p.name == "S'":
                        # Start symbol. Accept!
                        action[st,"$"] = 0
                        actionp[st,"$"] = p
                    else:
                        # We are at the end of a production.  Reduce!
                        for a in Follow[p.name]:
                            actlist.append((a,p,"reduce using rule %d" % p.number))
                            r = action.get((st,a),None)
                            if r is not None:
                                # Whoa. Have a shift/reduce or reduce/reduce conflict
                                if r > 0:
                                    # Need to decide on shift or reduce here
                                    # By default we favor shifting. Need to add
                                    # some precedence rules here.
                                    sprec,slevel = Productions[actionp[st,a].number].prec                                    
                                    rprec,rlevel = Precedence.get(a,('right',0))
                                    if (slevel < rlevel) or ((slevel == rlevel) and (rprec == 'left')):
                                        # We really need to reduce here.  
                                        action[st,a] = -p.number
                                        actionp[st,a] = p
                                        if not slevel and not rlevel:
                                            _vfc.write("shift/reduce conflict in state %d resolved as reduce.\n" % st)
                                            _vf.write("  ! shift/reduce conflict for %s resolved as reduce.\n" % a)
                                            n_srconflict += 1
                                    else:
                                        # Hmmm. Guess we'll keep the shift
                                        if not slevel and not rlevel:
                                            _vfc.write("shift/reduce conflict in state %d resolved as shift.\n" % st)
                                            _vf.write("  ! shift/reduce conflict for %s resolved as shift.\n" % a)
                                            n_srconflict +=1                                    
                                elif r < 0:
                                    # Reduce/reduce conflict.   In this case, we favor the rule
                                    # that was defined first in the grammar file
                                    oldp = Productions[-r]
                                    pp = Productions[p.number]
                                    if oldp.line > pp.line:
                                        action[st,a] = -p.number
                                        actionp[st,a] = p
                                    # print "Reduce/reduce conflict in state %d" % st
                                    n_rrconflict += 1
                                    _vfc.write("reduce/reduce conflict in state %d resolved using rule %d.\n" % (st, actionp[st,a].number))
                                    _vf.write("  ! reduce/reduce conflict for %s resolved using rule %d.\n" % (a,actionp[st,a].number))
                                else:
                                    print "Unknown conflict in state %d" % st
                            else:
                                action[st,a] = -p.number
                                actionp[st,a] = p
                else:
                    i = p.lr_index
                    a = p.prod[i+1]       # Get symbol right after the "."
                    if Terminals.has_key(a):
                        g = lr0_goto(I,a)
                        j = _lr0_cidhash.get(id(g),-1)
                        if j >= 0:
                            # We are in a shift state
                            actlist.append((a,p,"shift and go to state %d" % j))
                            r = action.get((st,a),None)
                            if r is not None:
                                # Whoa have a shift/reduce or shift/shift conflict
                                if r > 0:
                                    if r != j:
                                        print "Shift/shift conflict in state %d" % st
                                elif r < 0:
                                    # Do a precedence check.
                                    #   -  if precedence of reduce rule is higher, we reduce.
                                    #   -  if precedence of reduce is same and left assoc, we reduce.
                                    #   -  otherwise we shift
                                    rprec,rlevel = Productions[actionp[st,a].number].prec
                                    sprec,slevel = Precedence.get(a,('right',0))
                                    if (slevel > rlevel) or ((slevel == rlevel) and (rprec != 'left')):
                                        # We decide to shift here... highest precedence to shift
                                        action[st,a] = j
                                        actionp[st,a] = p
                                        if not slevel and not rlevel:
                                            n_srconflict += 1
                                            _vfc.write("shift/reduce conflict in state %d resolved as shift.\n" % st)
                                            _vf.write("  ! shift/reduce conflict for %s resolved as shift.\n" % a)
                                    else:                                            
                                        # Hmmm. Guess we'll keep the reduce
                                        if not slevel and not rlevel:
                                            n_srconflict +=1
                                            _vfc.write("shift/reduce conflict in state %d resolved as reduce.\n" % st)
                                            _vf.write("  ! shift/reduce conflict for %s resolved as reduce.\n" % a)
                                            
                                else:
                                    print "Unknown conflict in state %d" % st
                            else:
                                action[st,a] = j
                                actionp[st,a] = p
                                
            except StandardError,e:
                print "Hosed in slr_parse_table", e
                pass

        # Print the actions associated with each terminal
        if debug:
          for a,p,m in actlist:
            if action.has_key((st,a)):
                if p is actionp[st,a]:
                    _vf.write("    %-15s %s\n" % (a,m))
          _vf.write("\n")
          for a,p,m in actlist:
            if action.has_key((st,a)):
                if p is not actionp[st,a]:
                    _vf.write("  ! %-15s [ %s ]\n" % (a,m))
            
        # Construct the goto table for this state
        nkeys = { }
        for ii in I:
            for s in ii.usyms:
                if Nonterminals.has_key(s):
                    nkeys[s] = None

        # Construct the goto table for this state
        for n in nkeys.keys():
            g = lr0_goto(I,n)
            j = _lr0_cidhash.get(id(g),-1)            
            if j >= 0:
                goto[st,n] = j

        st += 1

    if n_srconflict == 1:
        print "pyson: %d shift/reduce conflict" % n_srconflict
    if n_srconflict > 1:
        print "pyson: %d shift/reduce conflicts" % n_srconflict        
    if n_rrconflict == 1:
        print "pyson: %d reduce/reduce conflict" % n_rrconflict
    if n_rrconflict > 1:
        print "pyson: %d reduce/reduce conflicts" % n_rrconflict


# -----------------------------------------------------------------------------
#                       ==== LALR(1) Parsing ====
# -----------------------------------------------------------------------------


# Compute the lr1_closure of a set I.  I is a list of tuples (p,a) where
# p is a LR0 item and a is a terminal

_lr1_add_count = 0

def lr1_closure(I):
    global _lr1_add_count

    _lr1_add_count += 1

    J = I[:]

    # Loop over items (p,a) in I.
    ji = 0
    while ji < len(J):
        p,a = J[ji]
        #  p = [ A -> alpha . B beta]

        #  For each production B -> gamma 
        for B in p.lr1_after:
            f = tuple(p.lr1_beta + (a,))

            # For each terminal b in first(Beta a)
            for b in first(f):
                # Check if (B -> . gamma, b) is in J
                # Only way this can happen is if the add count mismatches
                pn = B.lr_next
                if pn.lr_added.get(b,0) == _lr1_add_count: continue
                pn.lr_added[b] = _lr1_add_count
                J.append((pn,b))
        ji += 1

    return J

def lalr_parse_table():

    # Compute some lr1 information about all of the productions
    for p in LRitems:
        try:
            after = p.prod[p.lr_index + 1]
            p.lr1_after = Prodnames[after]
            p.lr1_beta = p.prod[p.lr_index + 2:]
        except LookupError:
            p.lr1_after = [ ]
            p.lr1_beta = [ ]
        p.lr_added = { }

    # Compute the LR(0) items
    C = lr0_items()
    CK = []
    for I in C:
        CK.append(lr0_kernel(I))

    print CK
    
# -----------------------------------------------------------------------------
#                          ==== LR Utility functions ====
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# _lr_write_tables()
#
# This function writes the LR parsing tables to a file
# -----------------------------------------------------------------------------

def lr_write_tables(modulename=tab_module):
    filename = modulename + ".py"
    try:
        f = open(filename,"w")

        f.write("""
# %s
# This file is automatically generated. Do not edit.

_lr_method = %s

_lr_signature = %s

_lr_action = %s

_lr_goto = %s

""" % (filename, repr(_lr_method), repr(Signature.digest()), repr(_lr_action),repr(_lr_goto)))
        f.close()

    except IOError,e:
        print "Unable to create '%s'" % filename
        print e
        return

def lr_read_tables(module=tab_module):
    global _lr_action, _lr_goto
    try:
        exec "import %s as parsetab" % module
        
        if Signature.digest() == parsetab._lr_signature:
            _lr_action = parsetab._lr_action
            _lr_goto   = parsetab._lr_goto
            return 1
        else:
            return 0
        
    except (ImportError,AttributeError):
        return 0


# -----------------------------------------------------------------------------
# yacc(module)
#
# Build the parser module
# -----------------------------------------------------------------------------

def yacc(method=default_lr, module=None, tabmodule=tab_module):
    initialize_vars()
    files = { }
    
    error = 0

    # Try to figure out what module we are working with
    if module:
        # User supplied a module object.
        if not isinstance(module, types.ModuleType):
            raise ValueError,"Expected a module"
        
        ldict = module.__dict__
        
    else:
        # No module given.  We might be able to get information from the caller.
        # Throw an exception and unwind the traceback to get the globals
        try:
            raise RuntimeError
        except RuntimeError:
            e,b,t = sys.exc_info()
            f = t.tb_frame
            f = f.f_back           # Walk out to our calling function
            ldict = f.f_globals    # Grab its globals dictionary
        
    # Get the tokens map
    tokens = ldict.get("tokens",None)
    if not tokens:
        raise PysonError,"module does not define a list 'tokens'"
    if not (isinstance(tokens,types.ListType) or isinstance(tokens,types.TupleType)):
        raise PysonError,"tokens must be a list or tuple."


    # Check to see if a requires dictionary is defined.
    requires = ldict.get("requires",None)
    if requires:
        if not (isinstance(requires,types.DictType)):
            raise PysonError,"requires must be a dictionary."

        for r,v in requires.items():
            try:
                if not (isinstance(v,types.ListType)):
                    raise TypeError
                v1 = [x.split(".") for x in v]
                Requires[r] = v1
            except StandardError:
                print "Invalid specification for rule '%s' in requires. Expected a list of strings" % r            

        
    # Build the dictionary of terminals.  We a record a 0 in the
    # dictionary to track whether or not a terminal is actually
    # used in the grammar

    if 'error' in tokens:
        print "pyson: Illegal token 'error'.  Is a reserved word."
        raise PysonError,"Illegal token name"

    for n in tokens:
        if Terminals.has_key(n):
            print "pyson: Warning. Token '%s' multiply defined." % n
        Terminals[n] = [ ]

    Terminals['error'] = [ ]

    # Get the precedence map (if any)
    prec = ldict.get("precedence",None)
    if prec:
        if not (isinstance(prec,types.ListType) or isinstance(prec,types.TupleType)):
            raise PysonError,"precedence must be a list or tuple."
        add_precedence(prec)
        Signature.update(repr(prec))

    for n in tokens:
        if not Precedence.has_key(n):
            Precedence[n] = ('right',0)         # Default, right associative, 0 precedence

    # Look for error handler
    ef = ldict.get('p_error',None)
    if ef:
        if not isinstance(ef,types.FunctionType):
            raise PysonError,"'p_error' defined, but is not a function."
        eline = ef.func_code.co_firstlineno
        efile = ef.func_code.co_filename
        files[efile] = None
        
        if (ef.func_code.co_argcount != 1):
            raise PysonError,"%s:%d. p_error() requires 1 argument." % (efile,eline)
        global Errorfunc
        Errorfunc = ef
    else:
        print "pyson: Warning. no p_error() function is defined."
        
    # Get the list of built-in functions with p_ prefix
    symbols = [ldict[f] for f in ldict.keys()
               if (isinstance(ldict[f],types.FunctionType) and ldict[f].__name__[:2] == 'p_'
                   and ldict[f].__name__ != 'p_error')]

    # Check for non-empty symbols
    if len(symbols) == 0:
        raise PysonError,"no rules of the form p_rulename are defined."
    
    # Sort the symbols by line number
    symbols.sort(lambda x,y: cmp(x.func_code.co_firstlineno,y.func_code.co_firstlineno))

    # Add all of the symbols to the grammar
    for f in symbols:
        if (add_function(f)) < 0:
            error += 1

    # Make a signature of the docstrings
    for f in symbols:
        Signature.update(f.__doc__)
    
    lr_init_vars()

    if error:
        raise PysonError,"Unable to construct parser."

    if not lr_read_tables(tabmodule):

        # Validate files
        for filename in files.keys():
            validate_file(filename)
        
        augment_grammar()    
        error = verify_productions()
        if error:
            raise PysonError,"Unable to construct parser."

        build_lritems()
        compute_first1()
        compute_follow()
        
        if method == 'SLR':
            slr_parse_table()
        elif method == 'LALR1':
            lalr_parse_table()
            return
        else:
            raise PysonError, "Unknown parsing method '%s'" % method
            
        lr_write_tables(tabmodule)        
    
        if debug:
            try:
                f = open(debug_file,"w")
                f.write(_vfc.getvalue())
                f.write("\n\n")
                f.write(_vf.getvalue())
                f.close()
            except IOError,e:
                print "pyson: can't create '%s'" % debug_file,e

# -----------------------------------------------------------------------------
#                              UTILITY FUNCTIONS
# -----------------------------------------------------------------------------

def is_identifier(s):
    for c in s:
        if not (c.isalnum() or c == '_'): return 0
    return 1


# -----------------------------------------------------------------------------
# LR parsing engine
#
# This code implements a generic shift-reduce LR parsing engine
# -----------------------------------------------------------------------------

# actions is a dictionary of actions of the form (type,number)
# goto is a dictionary of goto states

lookahead = None
lookaheadstack = [ ]

def pushback(t):
    global lookahead
    if lookahead:
        lookaheadstack.append(lookahead)
    lookahead = t
    
def parse(input = None, debug=0, lineno=-1, filename="",require=0):
    global lookahead, lookaheadstack
    actions = _lr_action
    goto    = _lr_goto
    prod    = Productions

    if input:
        plex.input(input)
        if lineno > 0:
            plex.lineno = lineno
        
    statestack = [ ]                # Stack of parsing states
    symstack   = [ ]                # Stack of grammar symbols
    lookahead  = None
    errtoken   = None               # Err token
    lookaheadstack = [ ]            # Stack of lookahead symbols

    # The start state is assumed to be (0,$)
    statestack.append(0)
    sym = PysonSymbol()
    sym.type = '$'
    symstack.append(sym)

    while 1:
        if not lookahead:
            if not lookaheadstack:
                lookahead = get_token()     # Get the next token
            else:
                lookahead = lookaheadstack.pop()
                
            if not lookahead:
                lookahead = PysonSymbol()
                lookahead.type = '$'
        if debug:
            print "%-20s : %s" % (lookahead, [xx.type for xx in symstack])

        # Check the action table
        s = statestack[-1]
        ltype = lookahead.type
        t = actions.get((s,ltype),None)

        if t is not None:
            if t > 0:
                # shift a symbol on the stack
                if ltype == '$':
                    # Error, end of input
                    print "Parse error. EOF"
                    return
                statestack.append(t)
                symstack.append(lookahead)
                lookahead = None
#                if len(symstack) > maxstack:
#                    print "pyson: maxstack exceeded"
#                    global maxstack
#                    maxstack = 10000000
                continue

            if t < 0:
                # reduce a symbol on the stack, emit a production
                p = prod[-t]
                pname = p.name
                plen  = p.len
            
                # Get production function
                sym = PysonSymbol()
                sym.type = pname       # Production name
                if plen:
                    targ = symstack[-plen-1:]
                    targ[0] = sym
                    del symstack[-plen:]
                    del statestack[-plen:]
                else:
                    targ = [ sym ]
                if reduce_func:
                    reduce_func(-t,p.func,targ)
                else:
                    p.func(targ)

                if require:
                    try:
                        t0 = targ[0]
                        r = Requires.get(t0.type,None)
                        t0d = t0.__dict__
                        if r:
                            for field in r:
                                tn = t0
                                for fname in field:
                                    try:
                                        tf = tn.__dict__
                                        tn = tf.get(fname)
                                    except StandardError:
                                        tn = None
                                    if not tn:
                                        print "%s:%d. Rule %s doesn't set required attribute '%s'" % \
                                              (p.file,p.line,p.name,".".join(field))
                    except TypeError,LookupError:
                        print "Bad requires directive " % r
                        pass
                    
                symstack.append(sym)
                statestack.append(goto[statestack[-1],pname])
                continue
        
            if t == 0:
                n = symstack[-1]
                if hasattr(n,"value"):
                    return n.value
                else:
                    return n

        if t == None:
            # We have some kind of parsing error here.  To handle this,
            # we are going to push the current token onto the tokenstack
            # and replace it with an 'error' token.  If any rule matches
            # this, it will catch it.

            # case 1:  the statestack only has 1 entry on it.  No error
            # was matched so we're totally hosed at this point.  We'll
            # call a user-defined error handler (if defined) and
            # discard the token

            if len(statestack) <= 1 and lookahead.type != '$':
                if not errtoken: errtoken = lookahead
                if Errorfunc:
                    lookahead = Errorfunc(errtoken)
                else:
                    if hasattr(errtoken,"lineno"): lineno = lookahead.lineno
                    else: lineno = 0
                    if lineno:
                        print "pyson: Syntax error at Line %d, token=%s" % (lineno,errtoken.type)
                    else:
                        print "pyson: Syntax error, token=%s" % errtoken.type
                    lookahead = None
                errtoken = None
                # Nuke the pushback stack
                del lookaheadstack[:]
                continue

            # case 2: the statestack has a couple of entries on it.
            # we nuke the top entry and generate an error token

            # Start nuking entries on the stack
            if lookahead.type == '$':
                # Whoa. We're really hosed here.
                if Errorfunc:
                    Errorfunc(None)
                else:
                    print "pyson: Parse error in input.  EOF"
                return 

            if lookahead.type != 'error':
                sym = symstack[-1]
                if sym.type == 'error':
                    # Hmmm. Error is on top of stack, we'll just nuke input
                    # symbol and continue
                    lookahead = None
                    continue
                t = PysonSymbol()
                t.type = 'error'
                t.value=None
                if hasattr(lookahead,"lineno"):
                    t.lineno = lookahead.lineno
                errtoken = lookahead
                t.value = errtoken
                pushback(t)
            else:
                symstack.pop()
                statestack.pop()
                
            continue

        # Call an error function here
        print "internal parser error!!!\n"
        break

    

    

        
        

    

        

        
    

