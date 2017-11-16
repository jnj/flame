#!/usr/bin/env python
#
# Run tests for project 4
#

import glob
import sys
import os


if len(sys.argv) < 2:
    print "Usage: python test4.py directory"
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
    print "Usage: python test4.py [-make] directory"
    raise SystemExit

if make:
    ext = "*.flm"
else:
    ext = "*.exp"

f = glob.glob("%s/%s" % (dirname,ext))

print "**** Running tests for Project 4 ****"

for t in f:
    name,suffix = t.split(".")
    failed = 0
    faileds = ""
    if make:
	if not os.path.exists("%s.exp" % name):
            os.system("python flame.py %s.flm >%s.build 2>&1" % (name,name))
            os.system("as %s.s >>%s.build 2>&1" % (name,name))
            os.system("ld %s.o flame.o -lc >>%s.build 2>&1" % (name,name))
            os.system("a.out <%s.in >%s.exp 2>&1" % (name,name))
    else:
        try:
            failed += os.system("python flame.py %s.flm >%s.build 2>&1" % (name,name))
            if not failed:
                failed += os.system("as %s.s >>%s.build 2>&1" % (name,name))
            if not failed:
                failed += os.system("ld %s.o flame.o -lc >>%s.build 2>&1" % (name,name))

            if failed:
                faileds += "Could not build %s.flm" % name
                try:
                    faileds += open("%s.build" % name).read()
                except:
                    pass
            else:
                os.system("a.out <%s.in >%s.out 2>&1" % (name,name))

                f = open("%s.exp" % name)
                exp = f.readlines()
                f.close()
                f = open("%s.out" % name)
                got = f.readlines()
                f.close()

#                print exp
#                print got
                
                if (len(exp) != len(got)):
                    faileds += "Output has a different number of lines"
                    failed = 1

                else:
                    for i in range(len(exp)):
                        if exp[i] != got[i]:
                            faileds += "expected: %s" % exp[i]
                            faileds += "got     : %s" % got[i]
                            failed = 1

        except StandardError,e:
            print e
            failed = 1
    print "Test %s %s" % (name, "."*(20-len(name))),
    if failed:
        print "failed! (see %s.dif for details)" % name
        f = open("%s.dif" % name, "w")
        f.write(faileds)
        f.close()
    else: print "passed"
            
        
        



        
    

