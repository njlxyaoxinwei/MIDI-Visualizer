visualize: visualize.lhs play.lhs
	ghc -O2 -optl-pthread $^
clean:
	rm -f *.o *.hi visualize
