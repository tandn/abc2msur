all:
	escript abc2msur.erl build
	erl -make

clean:
	rm *.beam parser.erl scanner.erl
