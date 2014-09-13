all: logappend logread

logread:
	gcc -march=native -O3  ../logReadSrc/* -lssl -lcrypto -lrt -o logread

logappend:
	gcc -march=native -O3 ../logAppendSrc/* -lssl -lcrypto -lrt -o logappend

clean:
	rm logappend
	rm logread
