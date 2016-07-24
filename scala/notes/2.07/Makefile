MAIN=
SRC=$(MAIN).scala
CLASSPATH=classes
CMPL=scalac
CFLG=-d
EXEC=scala
EFLG=-classpath

.PHONY: all compile run

all:
	@if [ -d $(CLASSPATH) ]; then make compile; \
	else mkdir $(CLASSPATH) && make compile; fi
	@make run

compile:
	$(CMPL) $(CFLG) $(CLASSPATH) $(SRC)

run:
	$(EXEC) $(EFLG) $(CLASSPATH) $(MAIN)
