# I made a makefile to let you know what common cabal commands are. You can use
# this or call these directly.
#
# More info about the makefile is in the project readme.
# -DD

default: build
basic: clean configure build
stack: clean configure-RTS build-stack
prof: clean configure-RTS build-prof

dependencies:
	cabal install --with-ghc=ghc-7.6.3 --only-dependencies

clean:
	cabal clean

configure:
	cabal configure --with-ghc=ghc-7.6.3 --enable-tests

configure-RTS:
	cabal configure --with-ghc=ghc-7.6.3 --enable-tests --enable-library-profiling --enable-executable-profiling

build:
	cabal build --with-ghc=ghc-7.6.3

build-stack:
	cabal build --with-ghc=ghc-7.6.3 --ghc-options="-auto-all -caf-all -prof -with-rtsopts=-xc"

build-prof-time:
	# see 7.14.4 of [http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/template-haskell.html]
	# for why this needs to be built twice
	cabal build --with-ghc=ghc-7.6.3
	cabal build --with-ghc=ghc-7.6.3 --ghc-options="-osuf p_o -auto-all -caf-all -prof -with-rtsopts=-p"

build-prof-heap:
	# see 7.14.4 of [http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/template-haskell.html]
	# for why this needs to be built twice
	cabal build --with-ghc=ghc-7.6.3
	cabal build --with-ghc=ghc-7.6.3 --ghc-options="-osuf p_o -auto-all -caf-all -prof -with-rtsopts=-h"

dist/build/logread/logread: build
dist/build/logappend/logappend: build
dist/build/test-bbf/test-bbf: build

test: dist/build/test-bbf/test-bbf
	cabal test

.PHONY: default basic stack prof dependencies clean configure configure-RTS build build-stack build-prof test logread logappend
