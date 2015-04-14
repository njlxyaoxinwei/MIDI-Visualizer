visualize: *.lhs *.hs
	ghc -O2 -optl-pthread $^
clean:
	rm -f *.o *.hi visualize
