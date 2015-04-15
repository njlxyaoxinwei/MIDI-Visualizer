visualize: *.lhs *.hs
	ghc -O2 -optl-pthread $^
clean:
	rm -rf *.o *.hi dist
