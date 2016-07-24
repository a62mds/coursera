MAIN=Main
SRCS=$(MAIN).scala Number.scala
CPTH=classes
CMPL=scalac
EXEC=scala

all:
	@make chkdir
	@make compile
	@make run

chkdir:
	@if ! [ -d $(CPTH) ]; then mkdir $(CPTH); fi

compile:
	@sbt compile

run:
	@sbt run
