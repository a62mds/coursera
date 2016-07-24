MAIN=Rationals
SRC=$(MAIN).scala
CLASSPATH=classes
CMPL=scalac
CFLAGS=-d
EXEC=scala
EFLAGS=-cp

.PHONY: all compile run

all:
	@if [ -d $(CLASSPATH) ]; then make compile; \
	else mkdir $(CLASSPATH) && make compile; fi
	@make run

compile:
	$(CMPL) $(CFLAGS) $(CLASSPATH) $(SRC)

run:
	$(EXEC) $(EFLAGS) $(CLASSPATH) $(MAIN)
