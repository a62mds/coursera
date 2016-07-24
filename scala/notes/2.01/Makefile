OBJ=Sum
SRC=$(OBJ).scala
CLSDIR=cls

CFLAGS=-d
RFLAGS=-classpath

all:
	scalac $(CFLAGS) $(CLSDIR) $(SRC)

run:
	scala $(RFLAGS) $(CLSDIR) $(OBJ)
