JBC=Main
SRC=$(JBC).scala

CLSPTH=classes

CMPL=scalac
CFLG=-d

EXEC=scala
EFLG=-cp

.DEFAULT: all

.PHONY: all chkdir compile run

all:
	@make chkdir
	@make compile
	@make run

chkdir:
	@if ! [ -d $(CLSPTH) ]; then mkdir $(CLSPTH); fi

compile:
	$(CMPL) $(CFLG) $(CLSPTH) $(SRC)

run:
	$(EXEC) $(EFLG) $(CLSPTH) $(JBC)
