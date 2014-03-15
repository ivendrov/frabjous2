
all : compiler
compiler: Compiler.hs Syntax.hs Parser.hs CodeGen.hs Transform.hs
	ghc Compiler Syntax Parser CodeGen Transform

clean:
	rm -rf *o *.hi *~ Syntax Parser CodeGen Transform 
