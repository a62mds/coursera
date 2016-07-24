JBC=tailRecFact
SRC=$(JBC).scala
CLSD=classes
CMPL=scalac
CFLG=-d
EXEC=scala
EFLG=-cp

all:
	@make chkdir
	@make compile
	@make run

chkdir:
	if ! [ -d $(CLSD) ]; then mkdir $(CLSD); fi 

compile:
	$(CMPL) $(CFLG) $(CLSD) $(SRC)

run:
	$(EXEC) $(EFLG) $(CLSD) $(JBC)
