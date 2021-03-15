Nonterminals lines line subsystem system process assignments
assignment action rpred spred apred expr vars arg_list arg_def_list
att_list att_init pcall init_list compinit compcall p_expr op_expr exprs.

Terminals
	'+' '-' '++' '--' '(' ')' '>' '<' '/=' '>=' '<=' '=' ':' ':=' '::=' 'and' 'or' 'not' '|' '||' '.' ',' '@' '[' ']' '{' '}' '->' '=>' 'notin' 'in' '*' '/'
	int self atom true false tt ff name literal param 'component' 'attributes' 'observables' 'behaviour' 'let' 'init' 'end' 'head' 'tail' 'nil' 'SYS'.

Rootsymbol lines.

Left 1 '|'.
Left 2 '+' '-' '++' '--'.
Left 3 '*' '/' '.' '>' '<' '[' ']'.
Left 4 'or'.
Left 5 'and'.
Right 6 'not'.
Left 7 '/=' '>=' '<=' '='.

lines -> line lines : ['$1' | '$2'].
lines -> line : ['$1'].

line -> 'component' name lines 'end' : {comp, v('$2'), '$3'}.
line -> 'attributes' ':' att_list : {attrs, '$3'}.
line -> 'observables' ':' att_list : {obsrs, '$3'}.
line -> 'behaviour' ':' 'let' '{' lines '}' 'init' process : {beh, '$5', {init_beh, '$8'}}.
line -> name ':' compinit : {comp_init, v('$1'), '$3'}.

compinit -> name : {comp, v('$1'), []}.
compinit -> name '(' init_list ')' : {comp, v('$1'), '$3'}.

init_list -> att_init : ['$1'].
init_list -> att_init ',' init_list : ['$1' | '$3'].

att_init -> literal '->' expr : {v('$1'),'$3'}.
att_init -> literal '=>' expr : {v('$1'),'$3'}.

line -> name ':=' process : {def, v('$1'), [], '$3'}.
line -> name '(' arg_def_list ')' ':=' process : {def, v('$1'), '$3', '$6'}.

line -> system : '$1'.

system -> 'SYS' '::=' subsystem : {sys,'$3'}.
subsystem -> compcall : ['$1'].
subsystem -> compcall '||' subsystem : ['$1' | '$3'].

compcall -> name : {comp_call, v('$1'), []}.
compcall -> name '(' init_list ')' : {comp_call, v('$1'), '$3'}.

process -> action '.' process : {prefix, {'$1', []}, '$3'}. % prefix , no update
process -> action '.' '[' assignments ']' process : {prefix, {'$1', '$4'}, '$6'}. % prefix, and update
process -> '<' apred '>' process : {p_awareness, '$2', '$4'}.
process -> process '|' process : {par, '$1', '$3'}.
process -> process '+' process : {choice, '$1', '$3'}.
process -> 'nil' : 'nil'.
process -> pcall : '$1'.
process -> '(' process ')' : '$2'.

action ->  '(' ')' '@' '(' false ')': {output, empty, empty}.
action ->  '(' exprs ')' '@' '(' spred ')': {output, '$2', '$6'}.
action ->  '(' rpred ')' '(' vars ')': {input, '$2', '$5'}.


%% Awareness predicate
apred -> '(' apred ')' : {parenthesis,'$2'}.
apred -> p_expr : '$1'.
apred -> apred 'and' apred : {'and', '$1', '$3'}.
apred -> apred 'or' apred : {'or', '$1', '$3'}.
apred -> 'not' apred : {'not', '$2'}.

%% Sending predicate
spred -> '(' spred ')' : {parenthesis,'$2'}.
spred -> p_expr : '$1'.
spred -> spred 'and' spred : {'and', '$1', '$3'}.
spred -> spred 'or' spred : {'or', '$1', '$3'}.
spred -> 'not' spred : {'not', '$2'}.

%% Receiving predicate

rpred -> '(' rpred ')' : {parenthesis,'$2'}.
rpred -> p_expr : '$1'.
rpred -> rpred 'and' rpred : {'and', '$1', '$3'}.
rpred -> rpred 'or' rpred : {'or', '$1', '$3'}.
rpred -> 'not' rpred : {'not', '$2'}.

p_expr -> expr '>=' expr : {'>=', '$1', '$3'}.
p_expr -> expr '<=' expr : {'<=', '$1', '$3'}.
p_expr -> expr '=' expr : {'=', '$1', '$3'}.
p_expr -> expr '>' expr : {'>', '$1', '$3'}.
p_expr -> expr '<' expr : {'<', '$1', '$3'}.
p_expr -> expr '/=' expr : {'/=', '$1', '$3'}.
p_expr -> expr 'in' expr : {ismember, '$1', '$3'}.
p_expr -> expr 'notin' expr : {notmember, '$1', '$3'}.
p_expr -> true : "true".
p_expr -> false : "false".

exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

expr -> int : {const,v('$1')}.
expr -> '-' int : {minusconst,v('$2')}.
expr -> tt : "true".
expr -> ff : "false".
expr -> atom : {token, v('$1')}.
expr -> literal '(' arg_list ')' : {func, v('$1'), '$3'}.
expr -> param : {param, v('$1')}.
expr -> literal : {literal,v('$1')}.
expr -> self : {self, v('$1')}.
expr -> op_expr : '$1'.
expr -> '|' expr '|' : {length, '$2'}.
expr -> expr '[' expr ']' : {selector, '$1', '$3'}.
expr -> expr '.' 'head' : {head, '$1'}.
expr -> expr '.' 'tail' : {tail, '$1'}.
expr -> '[' exprs ']' : {bracket, '$2'}.
expr -> '{' exprs '}' : {bracket2, '$2'}.
expr -> '[' ']' : empty_vector.
expr -> '{' '}' : empty_set.
%expr -> '[' expr ']' : {bracket, '$2'}.
%expr -> '{' expr '}' : {bracket2, '$2'}.


op_expr -> expr '++' expr : {'++','$1','$3'}.
op_expr -> expr '--' expr : {'--','$1','$3'}.
op_expr -> expr '+' expr : {'+','$1','$3'}.
op_expr -> expr '-' expr : {'-','$1','$3'}.
op_expr -> expr '/' expr : {'/','$1','$3'}.
op_expr -> expr '*' expr : {'*','$1','$3'}.

assignments -> assignment : ['$1'].
assignments -> assignment ',' assignments : ['$1' | '$3'].

assignment -> expr ':=' expr : {'$1', '$3'}.

vars -> literal : [v('$1')].
vars -> literal ',' vars : [v('$1') | '$3'].

pcall -> name : {call, v('$1'), []}.
pcall -> name '(' arg_list ')': {call, v('$1'), '$3'}.

arg_def_list -> literal : [v('$1')].
arg_def_list -> literal ',' arg_def_list : [v('$1') | '$3'].

arg_list -> expr ',' arg_list : ['$1' | '$3'].
arg_list -> expr : ['$1'].

att_list -> literal : [v('$1')].
att_list -> literal ',' att_list : [v('$1') | '$3'].

Erlang code.

-export([scan_and_parse/1]).

v({_, _, Value}) -> Value.

scan_and_parse(Code) ->
    case scanner:string(Code) of
	{ok, Tokens, _} ->
	    parse(Tokens);
	Err -> Err
    end.
