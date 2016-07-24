
MAIN=tailRecArith
CLASS=tailRecArith.scala
CLASSPATH=classes

CFLAGS=-d
RFLAGS=-classpath

all:
	@if [ -d $(CLASSPATH) ]; then make compile; \
	else mkdir $(CLASSPATH) && make compile; fi

compile:
	scalac $(CFLAGS) $(CLASSPATH) $(CLASS)

run:
	scala $(RFLAGS) $(CLASSPATH) $(MAIN)
