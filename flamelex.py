# Flame lexical analysis module
# (C) 2001 by Josh Joyce <jnjoyce@uchicago.edu>
# $Id: flamelex.py,v 1.2 2001/07/08 02:28:18 josh Exp $


import plex
import re
import string
import symtab


# Define a list of all of the valid token types
tokens = (
    'ID',
    'INUMBER',
    'FNUMBER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
    'EQUALS',
    'NEQUAL',
    'ASSIGN',
    'COLON',
    'SEMI',
    'COMMA',
    'GT',
    'LT',
    'GEQ',
    'LEQ',
    'AND',
    'OR',
    'NOT',
    'LBRACE',
    'RBRACE',
    'STRING',
    'FUN',
    'BEGIN',
    'END',
    'INT',
    'FLOAT',
    'WHILE',
    'DO',
    'IF',
    'THEN',
    'ELSE',
    'PRINT',
    'WRITE',
    'READ',
    'RETURN',
    'SKIP',
    'BREAK'
    )

# Define a dictionary of reserved words and the corresponding token types
keywords = {
    'do' : 'DO',
    'if' : 'IF',
    'or' : 'OR',
    'and' : 'AND',
    'not' : 'NOT',
    'fun' : 'FUN',
    'end' : 'END',
    'int' : 'INT',
    'then' : 'THEN',
    'else' : 'ELSE',
    'skip' : 'SKIP',
    'read' : 'READ',
    'begin' : 'BEGIN',
    'float' : 'FLOAT',
    'while' : 'WHILE',
    'print' : 'PRINT',
    'write' : 'WRITE',
    'break' : 'BREAK',
    'return' : 'RETURN'
    }

# Define the simple tokens of the language here.  You can do this
# just by defining strings of the form l_TOKNAME = r'regex' where
# regex is a regular expression defining the token.	 Note: some
# symbols like +,*,(,) have special meanings in regular expressions
# and have to be escaped with a backslash if you really want to
# match a character such as '*'.  Any valid Python regular expression
# should be acceptable
l_PLUS	  = r'\+'
l_MINUS	  = r'-'
l_TIMES	  = r'\*'
l_DIVIDE  = r'/(?!\*)'
l_LPAREN  = r'\('
l_RPAREN  = r'\)'
l_EQUALS  = r'=='
l_NEQUAL  = r'!='
l_ASSIGN  = r':='
l_COLON	  = r':'
l_SEMI	  = r';'
l_COMMA	  = r','
l_GT	  = r'>'
l_LT	  = r'<'
l_GEQ	  = r'>='
l_LEQ	  = r'<='
l_LBRACE  = r'\['
l_RBRACE  = r'\]'
l_STRING  = r'"([^\n\\]|(\\(n|"|\\)))*?"'

# Define more complex regular expression rules here.  In this case,
# functions of the form l_TOKNAME are defined to handle a token.
# The input to each of these rules is a token containing the parameters
# t.type, t.value, and t.lineno.  A rule should return the token if it
# wants to keep it, otherwise it should return nothing (in which case,
# the token is discarded and the next token read).

def l_FNUMBER(t):
    # Floating point literal
    r'\d+((\.\d+)([Ee][-\+]?\d+)?|([Ee][-\+]?\d+))'
    # Handle badly formed or otherwise invalid fp literals
    t0 = t.value[0]
    t1 = t.value[1]
    if t0 == '0' and t1 != 'e' and t1 != '.':
        # Float literal had invalid leading zero
        print "Line %i. Malformed floating point number '%s'" % \
              (t.lineno, t.value)
        return None
    # Otherwise, the token is fine
    return t

def l_INUMBER(t):
    # Integer literal
    r'\d+'
    # Handle badly formed integer literals
    if len(t.value) > 1 and t.value[0] == "0":
        # Leading zero
        print "Line %i. Malformed integer '%s'" % (t.lineno, t.value)
        return None
    # Accept it otherwise
    return t

def l_ID(t):
    # Identifier
    r'[a-zA-Z_]\w*'
    # Change its type if it's a keyword
    t.type = keywords.get(t.value, t.type)
    # If it is an identifier,
    if t.type == 'ID':
        # Try to look it up in the current symbol table
        t.symtab = symtab.local_lookup(t.value)
        if not t.symtab:
            # Add an entry if it is not there
            #t.symtab = symtab.global_lookup(t.value)
            #symtab.add_symbol(t.value, t.symtab)
            #if not t.symtab:
            t.symtab = symtab.add_new_symbol(t.value)
            #print "(%d) Lexer added symbol entry for %s" % (t.lineno, t.value)
            #print "(%d) %s symbol value is %s" % (t.lineno, t.value, str(t.symtab))
    return t
        
def l_comment(t):
    # Comment
    r'/\*(.|\n)*?\*/'
    # Fix the line number
    plex.lineno += t.value.count("\n")
    # Comments are not returned
    return None

def l_whitespace(t):
    # Whitespace
    r' (\s|\n)+ '
    # Fix the line number
    plex.lineno += t.value.count("\n")
    return None

# This rule is called if all else fails.  Your implementation
# should print some kind of sensible error message and try to
# recover. The plex.skip() function can be used to skip ahead
# in the input file and can be used to skip over illegal
# characters.  If you choose to do nothing, the lexer will fail
# with a ScanError exception.

# Precompile some re objects for use in errors
unknown_escape = re.compile(r'[^\n]*\\(?!\n)[^n"\\]')
quote_or_newline = re.compile(r'"|\n')

def l_error(t):
    tlen = len(t.value)
    if t.value[:2] == '/*':
        # Unterminated comment
        print "Line %i. Unterminated comment." % (t.lineno)
        plex.skip(tlen + 1)
    elif t.value[0] == '"':
        # Some kind of string-related error has occurred
        bad_esc = unknown_escape.match(t.value)
        if bad_esc:
            # Unknown escape sequence
            print "Line %i." % (t.lineno),
            print "Unsupported character escape %s in string literal." \
                  % (bad_esc.group()[-2:])
            # Now we need to skip to a sensible location
            # We look for another quote or a newline from where we matched
            found = quote_or_newline.search(t.value[bad_esc.end():])
            if not found:
                plex.skip(tlen + 1)
                return None
            plex.skip(found.start() + bad_esc.end())
            # If we skipped to the end quote, skip past it, too
            if t.value[found.start() + bad_esc.end()] == '"':
                plex.skip(1)
                return None
        else:
            # Unterminated quote
            print "Line %i. Unterminated string literal." % (t.lineno)
            pos = string.find(str(t.value), "\n", 1)
            if pos == -1:
                plex.skip(tlen + 1)
                return None
            # Skip to the end of the line
            plex.skip(pos)
            return None
    else:
        # Unknown error
        # Hmmm. Try to skip the current character
        print "Line %i. Illegal character '%s'" % (t.lineno, t.value[0])
        plex.skip(1)
        return None

# This call builds the lexer based on all of the rules you
# have defined above.
plex.lex()

# This code tests the lexer if it is being run as a main program.
# For instance:
#
#		python lexer.py filename
#

if __name__ == '__main__':
    plex.runmain()
