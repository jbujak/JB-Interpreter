SHELL=/bin/bash
GRAMMAR_FILES=ParGrammar.hs,PrintGrammar.hs,AbsGrammar.hs,LexGrammar.hs,ErrM.hs
OUT=interpreter

interpreter: ParGrammar.hs Interpreter.hs
	ghc Interpreter.hs -o $(OUT)

ParGrammar.hs: bnfc/Grammar.cf
	cd bnfc && bnfc -m --haskell Grammar.cf && $(MAKE)
	cp bnfc/{$(GRAMMAR_FILES)} ./

tags: *.hs
	hasktags .
	rm TAGS
	mv ctags tags

.PHONY: clean
clean:
	-cd bnfc && $(MAKE) distclean
	-rm {$(GRAMMAR_FILES)} 2>/dev/null
	-rm *.hi 2>/dev/null
	-rm *.o 2>/dev/null
	-rm $(OUT) 2>/dev/null

.PHONY: test
test:
	./test.sh
