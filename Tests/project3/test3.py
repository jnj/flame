#!/usr/bin/env python
#
# Run tests for project 3
#

import glob
import sys
import os


if len(sys.argv) < 2:
    print "Usage: python test3.py directory"
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
    print "Usage: python test3.py [-make] directory"
    raise SystemExit

if make:
    ext = "*.flm"
else:
    ext = "*.exp"

#f = glob.glob(ext)
f = glob.glob("%s/%s" % (dirname,ext))

print "**** Running tests for Project 3 ****"

for t in f:
    name,suffix = t.split(".")
    failed = 0
    faileds = ""
    if make:
	if not os.path.exists("%s.exp" % name):
	   os.system("python flameparse.py %s.flm >%s.exp" % (name,name))
    else:
      if os.system("python flameparse.py %s.flm >%s.out 2>&1" % (name,name)):
        failed  = 1
      else:
        try:
            f = open("%s.exp" % name)
            exp = f.readlines()
            f.close()
            f = open("%s.out" % name)
            got = f.readlines()
            f.close()
            
            if (len(exp) == 0) and len(got) and got[0][:4] != "::::":
                faileds = "Expected no errors, but got the following:\n"
                faileds += "".join(got)
                failed = 1

            if len(exp):
                for i in exp:
                    if i not in got:
                        faileds += "expected: %s" % i
                        failed = 1
                        
        except StandardError:
            failed = 1
    print "Test %s %s" % (name, "."*(20-len(name))),
    if failed:
        print "failed! (see %s.dif for details)" % name
        f = open("%s.dif" % name, "w")
        f.write(faileds)
        f.close()
    else: print "passed"
            
        
        



        
    

