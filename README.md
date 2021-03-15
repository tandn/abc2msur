abc2msur
=====

The src folder contains the translator which takes an abc specification as input and produces  msur program as output



Build
-----
    $ rebar3 compile

Translation
----

Make sure that your abc specification has the name of "file.abc"

The command

> erl -eval "abc2msur:file(\"file.abc\")" -run init stop -noshell

will produce a Msur specification with the name "file.msur".

Verification
----

By using the toolset supporting verification of msur specifications (which eventually utilize state-of-the-art C verifiers)
https://github.com/labs-lang/masseur
