all:
	cabal update
	(cd .. ; $(MAKE) dependencies clean configure build)
	cp ../dist/build/logappend/logappend .
	cp ../dist/build/logread/logread .

quick:
	(cd .. ; $(MAKE) build)
	cp ../dist/build/logappend/logappend .
	cp ../dist/build/logread/logread .
