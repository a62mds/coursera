MAIN=Main
SRC=$(MAIN).scala
CPTH=classes

EXEC=scala
CMPL=scalac

.PHONY: all compile chkdir

all:
	@make chkdir
	@make compile
	@make run

run:
	$(EXEC) -cp $(CPTH) $(MAIN)

compile:
	$(CMPL) -d $(CPTH) $(SRC)

chkdir:
	@if ! [ -d $(CPTH) ]; then mkdir $(CPTH); fi
