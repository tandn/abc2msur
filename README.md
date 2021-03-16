abc2msur
=====

The src folder contains the translator which takes an abc specification as input and produces  msur program as output



Build
-----
    $ rebar3 compile

This results in a folder _build where beam files are located.

Translation
----

Make sure that your abc specification has the name of "file.abc"

The command has the form of

> erl -pa _build/default/lib/abc2msur/ebin/ -eval "abc2msur:file(\"examples/file.abc\")" -run init stop -noshell

will produce a Msur specification with the name "file.msur" in the same location of abc file.

Verification
----

By using the toolset supporting verification of msur specifications (which eventually utilize state-of-the-art C verifiers)
https://github.com/labs-lang/masseur
