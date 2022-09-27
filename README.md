
http://lichteblau.blogspot.com/2007/08/cloak.html

2007-08-26
cloak

CLOAK is a (somewhat unfinished) JVM written in Lisp, using GNU Classpath as its class library and SBCL as the underlying Lisp implementation.

It works by translating .class files into Lisp sources and running them through SBCL's compiler.

I wrote it years ago and never finished it or released what I had, but recently there was some talk about JVMs on comp.lang.lisp, so I decided to make it available as-is.

There's no link to a home page with more details because there is no home page. ;-)


It is unfinished, slow, buggy, unmaintained, in need of a rewrite -- and now you can hack it yourself!

Doesn't that sound exciting?  Of course it does.

Prerequisites. Only Linux/x86 is supported1. You will need several hours of spare CPU time, about 1 GB of RAM, and lots of disk space. Compilation involves building SBCL and classpath first, so make sure to install all required dependencies first. Debian users can run

# apt-get install sbcl svn cvs wget jikes
# apt-get build-dep classpath

to do so.

Build script. Grab cloak using git [edit: needs git 1.5, no idea why]:

$ git clone http://www.lichteblau.com/git/cloakbuild.git

and compile it using clbuild-like commands:

$ ./build update
$ ./build world

Usage. The bin directory contains scripts called java, javac (courtesy of ecj), javap, and javah that run Lisp with the right arguments.

$ ./bin/java -version
CLOAK Virtual Machine, running on SBCL 0.9.8.6 (Linux 2.6.22 X86)

Copyright (C) 2003-2007 David Lichteblau

Technically it is a precompiler, and to avoid unpleasant surprises at run time, you might want to run

./bin/precompile foo.jar

before starting anything non-trivial.

Finally, read cloak/TODO and start hacking.

What's new? Compared to the big binary tarball available previously, this one comes with sources only, has been updated for current SBCL, and for Classpath 0.91 (which is still ancient, but a little step forward). The scripts in bin/ are also new.


1 No AMD 64 support yet. For now, use an x86 chroot instead.
