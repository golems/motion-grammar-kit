# Makefile for motion-grammar-kit example code

LISP := sbcl --script
CC := gcc

# These are set to build in the ./src subdirectory of motion-grammar-kit
CFLAGS := -I../include -L../.libs -lmotgram -lach

# Run the example program
# LD_LIBRARY_PATH is set to run in the ./src subdirectory of motion-grammar-kit
default: example
	LD_LIBRARY_PATH=../.libs ./example

example: example.c load_parse.c
	$(CC) $(CFLAGS) -o example example.c

load_parse.c: example.lisp
	$(LISP) example.lisp

clean:
	rm load_parse.c super_load_parse.c super.dat example
