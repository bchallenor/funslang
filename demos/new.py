import os
import sys

if len(sys.argv) is not 2:
    print "please supply a project name"
else:
    n = sys.argv[1]
    
    f = open("demo.vcproj.template", "r")
    t = f.read()
    f.close()

    if not os.path.isdir(n):
        os.mkdir(n)
    
    g = open(n + "/" + n + ".vcproj", "w")
    g.write(t.replace("__projectname__", n))
    g.close()
