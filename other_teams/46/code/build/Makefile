CFLAGS=-std=c++11 -I. -Ofast -lcrypto -Wno-deprecated-declarations #-Weverything
FILES = $(shell for file in `ls src/*.cpp | grep -vE "logread|logappend|batch|main"`;do echo $$file; done)

all: logappend logread

logappend: src/logappend.cpp
	g++ ${FILES} src/logappend.cpp -o logappend ${CFLAGS}

logread: src/logread.cpp
	g++ ${FILES} src/logread.cpp -o logread ${CFLAGS}

clean:
	rm -f test_state logread logappend


test: test_state.cpp test_formatter.cpp

test_state.cpp:
	g++ ${CFLAGS} ${FILES} test/test_state.cpp -o test_state

test_formatter.cpp:
	g++ ${CFLAGS} ${FILES} test/test_formatter.cpp -o test_formatter

