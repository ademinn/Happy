all: Parser

Parser: Parser.hs Scanner.hs
	ghc --make Parser.hs

Parser.hs: Parser.y
	happy Parser.y

Scanner.hs: Scanner.x
	alex Scanner.x

clean:
	rm *~
	rm *.hi
	rm *.o
