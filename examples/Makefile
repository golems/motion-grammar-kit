LISP := sbcl --script
CC := gcc -I "../include" -lmotgram -L../.libs -lach

example: example.c load_parse.c
	$(CC) -o example example.c

load_parse.c: example.lisp
	$(LISP) example.lisp
