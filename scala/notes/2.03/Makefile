MAIN=fixedPoint
SRC=$(MAIN).scala

CMPL=scalac
CFLAGS=-d
CLASSPATH=classes
EXEC=scala
EFLAGS=-classpath

.PHONY: all compile run

all:
	@if [ -d $(CLASSPATH) ]; then make compile; \
	else mkdir $(CLASSPATH) && make compile; fi

compile: $(SRC)
	$(CMPL) $(CFLAGS) $(CLASSPATH) $(SRC)

run:
	$(EXEC) $(EFLAGS) $(CLASSPATH) $(MAIN)
