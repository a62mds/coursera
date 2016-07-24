JBC=Sqrt
SRC=$(JBC).scala

CMPL=scalac
CFLG=-d

EXEC=scala
EFLG=-cp

CLSP=cls

.PHONY: all compile run
.DEFAULT: all

all:
	@make chkdir
	@make compile
	@make run

chkdir:
	if ! [ -d $(CLSP) ]; then mkdir $(CLSP); fi

compile:
	$(CMPL) $(CFLG) $(CLSP) $(SRC)

run:
	$(EXEC) $(EFLG) $(CLSP) $(JBC)
