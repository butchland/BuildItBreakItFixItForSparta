all: logappend logread

logread:
	gcc -O3 logReadSrc/* -o build/logread -lssl -lcrypto

logappend:
	gcc -O3 logAppendSrc/* -o build/logappend -lssl -lcrypto

clean:
	rm build/logappend
	rm build/logread
