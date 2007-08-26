CC=cc
JAVAC=jikes
CFLAGS=-g
CLASSPATH=/usr/include/classpath

all: c/libcloak.so java


.PHONY: java
java:
	find java -name \*.java | xargs jikes -cp java:classpath:classpath.zip

c/jni_md.h: $(CLASSPATH)/jni_md.h
	cp $< $@

c/libcloak.so: c/libcloak.c c/jni_md.h
	$(CC) $(CFLAGS) -shared -o $@ -I. -I$(CLASSPATH) $<

.PHONY: clean clean2
clean:
	rm -f lisp/*.fasl lisp/native/*.fasl

clean2:
	rm -f *.so

clearcache:
	rm -rf cache-old
	mv cache cache-old
	mkdir cache
