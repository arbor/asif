all: t

ldopts.txt:
	stack exec -- perl opts.pl asif

incopts.txt:
	stack exec -- perl opts.pl asif

t: test.c ldopts.txt incopts.txt
	cc -o t test.c -Llib `cat ldopts.txt` `cat incopts.txt`

.PHONY: all
