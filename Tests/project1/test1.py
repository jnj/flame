#!/usr/bin/env python
#
# Run tests for project 1
#

import glob
import sys
import os


make = 0

if len(sys.argv) < 2:
    print "Usage: python test1.py directory"
    raise SystemExit

make = 0
dirname = None

for o in sys.argv[1:]:
    if o == '-make':
        make = 1
    else:
        dirname = o
        break

if not dirname:
    print "Usage: python test1.py [-make] directory"
    raise SystemExit

if make:
    ext = "*.flm"
else:
    ext = "*.exp"

#f = glob.glob(ext)
#f.extend(glob.glob("tests/"+ext))
f = glob.glob("%s/%s" % (dirname,ext))
print "**** Running tests for Project 1 ****"

for t in f:
    name,suffix = t.split(".")
    failed = 0
    faileds = ""
    if make:
	if not os.path.exists("%s.exp" % name):
	   os.system("python flamelex.py %s.flm >%s.exp" % (name,name))
    else:
        os.system("python flamelex.py %s.flm >%s.out" % (name,name))
        try:
            f = open("%s.exp" % name)
            exp = f.readlines()
            f.close()
            f = open("%s.out" % name)
            got = f.readlines()
            f.close()
            if len(exp) != len(got):
                failed = 1
                faileds = "Output has different number of lines.\n"
            else:
                # Compare line by line
                for i in range(len(exp)):
                    eline = exp[i]
                    if eline[0] == '(':
                        eline = eline[eline.index(","):]
                        try:
                            gline = got[i]
                            gline = gline[gline.index(","):]
                            if eline != gline:
                                faileds += "expected: (*%s" % eline
                                faileds += "got     : (*%s\n" % gline
                                failed = 1
                        except StandardError:
                            failed = 1
                    else:
                        if exp[i] != got[i]:
                            failed = 1
                            faileds += "expected: %s" % exp[i]
                            faileds += "got     : %s\n" % got[i]
        except StandardError:
            failed = 1
    print "Test %s %s" % (name, "."*(20-len(name))),
    if failed:
        print "failed! (see %s.dif for details)" % name
        f = open("%s.dif" % name, "w")
        f.write(faileds)
        f.close()
    else: print "passed"
            
        
        



        
    

