LISP := sbcl --script
CC := gcc

example: example.c load_parse.c
	$(CC) -o example example.c

load_parse.c: example.lisp
	$(LISP) example.lisp
