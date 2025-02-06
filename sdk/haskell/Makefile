BASE=~/datastar-haskell

.PHONY: all
all:
	@echo "This is just a big case statement for the following actions"
	@echo "test     - run the tests, with doctest"
	@echo "watch    - run ghciwatch, to point out your mistakes in real time"
	@echo "build    - build everything"
	@echo "haddock  - generate the Haddock documentation files"
	@echo "clean    - delete the dist-newstyle directory"
	@echo "tags     - generate a TAGS file, if you are not an emacs user, you should be"
	@echo "demo     - run the demo, have a look"
	@echo "hello    - run the ubiquitous hello world program, this time in color"
.PHONY:	
test:
	cabal repl lib:datastar --with-compiler=doctest 2>&1 | tee /tmp/datastar.log
#cabal repl lib:datastar --with-compiler=doctest --repl-options=--verbose 2>&1 | tee /tmp/e.log
.PHONY:
watch:
	ghciwatch --clear
.PHONY:
build:
	cabal build 
.PHONY:
haddock:
	cabal haddock --enable-documentation
.PHONY:
clean:
	rm -rf dist-newstyle
.PHONY:
tags:
	pushd  $(BASE)/src ; find . -name "*.hs" | grep -v "dist-newstyle" | xargs ghc-tags --etags -f $(BASE)/TAGS ; popd
.PHONY:
demo:
	cabal run exe:datastar-demo 2>&1 | tee /tmp/datastar.log
hello:
	cabal run exe:hello-world   2>&1 | tee /tmp/datastar.log

