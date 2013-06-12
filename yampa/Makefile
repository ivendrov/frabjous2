#makefile
APP='/usr/local/wxhaskell/bin/macosx-app'


tbtest:	esrd_tb.app
	open esrd_tb.app

esrd_tb.app:	esrd_tb
	$(APP) esrd_tb

esrd_tb: esrd_tb.hs tbGUI.hs
	ghc --make tbGUI.hs esrd_tb.hs -o esrd_tb


frabtest:	frab.app
	open frab.app

frab.app:	frab
	$(APP) frab

frab: frabjous.hs  frabTest.hs
	ghc --make frabTest.hs frabjous.hs -o frab
	



sirtest:	sir.app
	open sir.app

sir.app:	sir
	$(APP) sir

sir: sir.hs 
	ghc --make sir.hs 



prisonertest:	prisoner.app
	open prisoner.app

prisoner.app:	prisoner
	$(APP) prisoner

prisoner: prisonGUI.hs prisoner.hs 
	ghc --make prisonGUI.hs -o prisoner


lifetest:	game_of_life.app
	open game_of_life.app

game_of_life.app:	game_of_life
	$(APP) game_of_life

game_of_life: game_of_life.hs 
	ghc --make game_of_life.hs


multitest: multi.app
	open multi.app

multi.app: multi
	$(APP) multi	

multi: multi.hs 
	ghc --make multi.hs


wolftest: wolf.app
	open wolf.app

wolf.app: wolf
	$(APP) wolf	

wolf: wolf.hs LotkaVolterra.hs
	ghc --make wolf.hs


waretest: wareGUI.app
	open wareGUI.app

wareGUI.app: wareGUI
	$(APP) wareGUI	

wareGUI: wareGUI.hs Warehouse.hs
	ghc --make wareGUI.hs


fishtest: fish.app
	open fish.app

fish.app: fish
	$(APP) fish

fish: fish.hs
	ghc --make fish


orbittest: orbit.app
	open orbit.app

orbit.app: orbit
	 $(APP) orbit

orbit: orbit.hs
	ghc --make orbit

squeaky:	clean
	rm -rf *.app

clean:
	rm -f *.hi *.o

