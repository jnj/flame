# flame.py
# Driver script for Flame compiler
# Josh Joyce
# jnjoyce@uchicago.edu
# $Id: flame.py,v 1.1.1.1 2001/06/05 06:11:41 josh Exp $

import sys
import os
import flamegen
import flameparse

# Set the recursion limit
sys.setrecursionlimit(5000)

# Look for .flm files on the command line
f = filter(lambda arg: arg.find('.flm') != -1, sys.argv[1:])
if not f:
    # If none found, quit
    sys.exit()

# Compile the first .flm file on the command line
filename = f[0]
outname = os.path.splitext(filename)[0] + '.s'

# Open the file
f = open(filename)
data = f.read()
f.close()

# Run the parser and code generator
top = flameparse.parse(data)
if top:
    outf = open(outname, "w")
    flamegen.generate(outf, top)
    outf.close()

    # Print the parse tree if we get '-t' on the command line
    if '-t' in sys.argv:
        flameparse.dump_tree(top)

    # Run the assembler and linker
    name,suffix = filename.split(".")
    os.system("as %s.s" % (name))
    os.system("ld %s.o flame.o -lc" % (name))
