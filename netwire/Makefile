
all : output

compiler: Compiler.hs
	ghc Compiler

model: compiler top bottom thim.fj
	./Compiler < thim.fj > middle
	cat top middle bottom > model.hs

output: compiler model
	ghc model.hs
	./model <<< "30" > output2
	diff output1 output2

clean:
	rm -rf *o *.hi Compiler model.hs model
