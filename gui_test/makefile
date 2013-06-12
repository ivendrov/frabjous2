#Makefile for gui testing

gui.app: gui-base
	/usr/local/wxhaskell/bin/macosx-app gui

gui-base: 
	ghc -package wx gui.hs -o gui

