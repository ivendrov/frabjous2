
all : model

compiler: Compiler.hs Syntax.hs Parser.hs CodeGen.hs Transform.hs
	ghc Compiler Syntax Parser CodeGen Transform

model: compiler top bottom thim.fj
	./Compiler < thim.fj > middle
	cat top middle bottom > model.hs
	ghc model.hs

output: compiler model
	./model <<< "30" > output2
	diff output1 output2

clean:
	rm -rf *o *.hi *~ Compiler Syntax Parser CodeGen Transform  model.hs model middle output2
