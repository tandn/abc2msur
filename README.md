The folder contains the translator which takes an abc specification as input and produces an erlang program as output

# Compile the tool
> make

# Translation

Assume your abc spec has the name of "file.abc"

The command

> erl -eval "abc2abel:file(\"file.abc\")" -run init stop -noshell

will produce one or more Erlang modules, depending on how many component types are there in the abc specification.

# Execution

The generated *.erl files are meant to be complied and executed by ABEL, provided in another folder

For the latest ABEL impelementation, please consult https://github.com/ArBITRAL/ABEL
