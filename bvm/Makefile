all: bvm

# NOTE: -I/opt/homebrew/Cellar/bdw-gc/8.2.4/include -L/opt/homebrew/Cellar/bdw-gc/8.2.4/lib
bvm: bvm.c
	gcc bvm.c -o bvm -O3 -Wall -pedantic -lgc

clean:
	rm -f bvm
