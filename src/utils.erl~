-module(utils).
-export([build_update/3,    % evalulation of update, no vars
	 build_apred/2,     % evaluation of aware pred
	 build_spred/2,
	 build_rpred/3,
	 build_msg/2,
	 build_args/2]).

%% build_args of process call
%% A1 - process param
%% A2 - variables bound to messages
build_args(A1,A2) ->
    Index1 = lists:seq(0, length(A2)-1),
    Msg = lists:zip(A2, Index1),
    evalm(A1,Msg,[]).

evalm([],A2, Acc) ->
    string:join(lists:reverse(Acc),",");
evalm([H|T],A2,Acc) ->
    evalm(T,A2,[evalm(H,A2)|Acc]).

evalm({'+',L,R},A2) -> "( + " ++ evalm(L,A2) ++ evalm(R,A2) ++ ")";
evalm({'-',L,R},A2) -> "( - " ++ evalm(L,A2) ++ evalm(R,A2) ++ ")";
evalm({'*',L,R},A2) -> "( * " ++ evalm(L,A2) ++ evalm(R,A2) ++ ")";
evalm({literal,X},A2) ->
    io:format("what is A2 ~p~n",[A2]),
    case proplists:get_value(X,A2) of
	undefined ->
	    lists:flatten(io_lib:format("~p",[list_to_atom(X)]));
	    %"_" ++ string:uppercase(X);
	I ->
	    "\"msg[" ++ integer_to_list(I) ++ "]\""
    end;
evalm({const,X},_) ->
    X;
evalm({func,Name,L},A2) ->
    "user_code:" ++ Name ++ "(" ++ string:join([evalm(X,A2) || X <- L],",") ++ ")".




%% build updates
build_update(MyAtts,Upd, Msg) ->
    build_update(MyAtts,Upd,Msg,[]).

build_update(_,[],_,[]) -> "";
build_update(_,[],_,[{F,S}]) -> "(" ++ F ++ ")" ++ " " ++ "(" ++ S ++ ")";
build_update(_,[],_,Acc) ->
    Res = lists:reverse(Acc),
    "(list " ++ string:join([X || {X,_} <- Res]," ") ++ ")" ++ " " ++ "(list " ++ string:join([Y || {_,Y} <- Res]," ") ++ ")";
build_update(MyAtts,[{F,S}|T],M,Acc) ->
    build_update(MyAtts,T,M,[{evall(MyAtts,F,M),evall(MyAtts,S,M)}|Acc]).

build_apred(MyAtts,[]) ->
   "";
build_apred(MyAtts,Pred) ->
%   io:format("build aware pred ~p with b ~p ~n",[Pred,B]),
   evall(MyAtts,Pred,[]).


%% this functions returns code for subpredicates if there any (to deal with membership predicate) and code for the input predicate Pred
build_spred(_,[]) ->
   % io:format("build sending pred ~p ~n",[Pred]),
    %% create temporary names for variables
    "#t";
build_spred(OtherAtts,Pred) ->
   % io:format("build sending pred ~p ~n",[Pred]),
    %% create temporary names for variables
    evals(OtherAtts,Pred).

%% receiving predicates can refer to previous variables in Bound or new variables in the message
build_rpred(_,[], _) ->
    "#t";
build_rpred(OtherAtts, Pred, M) ->
%    "fun(_LclE, _M, _RmtE) -> msg_size(_M) == " ++ integer_to_list(length(M)) ++ " andalso " ++ evalr(OtherAtts, Pred, M) ++ " end".
    evalr(OtherAtts, Pred, M).

evall(_,{self,Att},_M) ->
    %"(this " ++ Att ++ ")";
    Att;
evall(_,{param,Att},_M) ->
    Att;
evall(Atts,{parenthesis,P},_M) ->
    "(" ++ evall(Atts,P,_M) ++ ")";
evall(Atts,{bracket,L},_M) ->
    "[" ++ string:join([evall(Atts,E,_M) || E <- L], ",") ++ "]";
evall(Atts,{bracket2,L},_M) ->
    "sets:from_list([" ++ string:join([evall(Atts,E,_M) || E <- L], ",") ++ "])";
evall(Atts,{head,Name},_M) -> "hd(" ++ evall(Atts,Name,_M) ++ ")";
evall(Atts,{tail,Name},_M) -> "tl(" ++ evall(Atts,Name,_M) ++ ")";
evall(Atts,{length, R},_M) ->
    "sets:size(" ++ evall(Atts,R,_M) ++ ")";
evall(_,"true",_M) -> "#t";
evall(_,"false",_M) -> "#f";
evall(_,empty_vector,_M) -> "[]";
evall(_,empty_set,_M) -> "sets:new()";
evall(_,{const,C},_M) -> C;
evall(_,{minusconst,C},_M) -> "(- 0 " ++ C ++ ")";
evall(_,{token,T},_M) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evall(Atts,[H|T] = L,_M) -> %really bracket of a list
    string:join([evall(Atts,X,_M) || X <- L],",");
evall(Atts,{concat,L,R},_M) ->
    evall(Atts,L,_M) ++ " + "  ++ evall(Atts,R,_M);
evall(Atts,{selector, L, I},_M)  ->
     {L1, I1, C} = flatten(L,I),
     "user_code:selector" ++ integer_to_list(C) ++ "(" ++ evall(Atts,L1,_M) ++ "," ++ string:join([evall(Atts,E,_M) || E <- I1], ",") ++ ")";
evall(Atts,{func,Name,L},_M) -> "user_code:" ++ Name ++ "(" ++ string:join([evall(Atts,C,_M) || C <- L],",") ++ ")";
evall(Atts,{'++',L,R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:union(" ++ L1 ++ "," ++ R1 ++ ")";
evall(Atts,{'--',L,R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:substract(" ++ L1 ++ "," ++ R1 ++ ")";
evall(Atts,[],_M) -> "[]";
evall(Atts,{'not', R},_M) ->
    " not " ++ evall(Atts,R,_M);
evall(Atts,{ismember, L, R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "sets:is_element("++L1++","++R1++")";
evall(Atts,{notmember, L, R},_M) ->
    L1 = evall(Atts,L,_M),
    R1 = evall(Atts,R,_M),
    "not " ++ "sets:is_element("++L1++","++R1++")";
evall(Atts,{literal, Name},_M) ->

    case lists:member(Name,Atts) of
	true ->
	    %"(this " ++ lists:flatten(io_lib:format("~p",[list_to_atom(Name)])) ++ ")";
	    lists:flatten(io_lib:format("~p",[list_to_atom(Name)]));
	false ->
	    %io:format("Eval literal~p with message ~p~n",[Name,_M]),
	    %% case proplists:get_value(Name,_M) of
	    %% 	undefined ->
	    %% 	    lists:flatten(io_lib:format("~p",[list_to_atom(Name)]));
	    %% 	    %"_" ++ string:uppercase(Name);
	    %% 	I ->
	    %% 	    "\"msg[" ++ integer_to_list(I) ++ "]\""
	    %% end
	    lists:flatten(io_lib:format("~p",[list_to_atom(Name)]))
    end;
evall(Atts,{Op, L, R},_M) ->
    "(" ++ atom_to_list(Op) ++ " " ++ evall(Atts,L,_M) ++ " " ++ evall(Atts,R,_M) ++ ")".

%% evaluation of eval send
evals(OtherAtts,{parenthesis,P}) ->
    "(" ++ evals(OtherAtts,P) ++ ")";
evals(OtherAtts,{bracket,L}) ->
    "[" ++ string:join([evals(OtherAtts,E) || E <- L], ",") ++ "]";
evals(OtherAtts,{bracket2,L}) ->
    "sets:from_list([" ++ string:join([evals(OtherAtts,E) || E <- L], ",") ++ "])";
evals(OtherAtts,{head,N}) -> evals(OtherAtts,N) ++ ".head";
evals(OtherAtts,{tail,N}) -> evals(OtherAtts,N) ++ ".tail";

%% NEED TO REVIEW
evals(_,"true") -> "#t";
evals(_,"false") -> "#f";
%% inside sending predicates, there is no variables (carried by msg as in case of receiving predicate.. this appear here because the parser cannot distinguish if a term is a variable or an attribtue of a different component.
evals(OtherAtts,{literal,Name}) ->
    case lists:member(Name,OtherAtts) of
	true ->
	        "(that " ++ Name ++ ")";
	false ->
	    lists:flatten(io_lib:format("~p",[list_to_atom(Name)]))
%	    "_" ++ string:uppercase(Name)
    end;
evals(OtherAtts,{self,Att}) ->
    %"(this " ++ Att ++ ")";
    Att;
evals(_,{param,Att}) ->
    Att;
evals(OtherAtts,{const,C}) -> C;
evals(OtherAtts,{minusconst,C}) -> "(- 0 " ++ C ++ ")";
evals(OtherAtts,{token,T}) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evals(OtherAtts,empty) -> "false";
evals(OtherAtts,[]) -> "[]";
evals(OtherAtts,[{self,Att}=Term]) ->
    "[" ++ evals(OtherAtts,Term) ++ "]";
evals(OtherAtts,[{var,Att}=Term]) ->
    "[" ++ evals(OtherAtts,Term) ++ "]";
evals(OtherAtts,{ismember, L, R}) ->
    L1= evals(OtherAtts,L),
    R1 = evals(OtherAtts,R),
    "sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evals(OtherAtts,{notmember, L, R}) ->
    "not " ++ evals(OtherAtts,{ismember, L, R});
evals(OtherAtts,[H|T]=List) when T =/= [] ->
    S = [evals(OtherAtts,Name) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";
evals(OtherAtts,{'not', T}) ->
    "(not " ++ evals(OtherAtts,T) ++ ")";
%% binary operations math + logical
%% evals(OtherAtts,{diff, L, R}) ->
%%     evals(OtherAtts,L) ++ " =/= " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{ge, L, R}) ->
%%     evals(OtherAtts,L) ++ " > " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{geq, L, R}) ->
%%     evals(OtherAtts,L) ++ " >= " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{leq, L, R}) ->
%%     evals(OtherAtts,L) ++ " =< " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{le, L, R}) ->
%%     evals(OtherAtts,L) ++ " < " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{'+',L,R}) ->
%%     evals(OtherAtts,L) ++ " + " ++ evals(OtherAtts,R);
%% evals(OtherAtts,{'*',L,R}) ->
%%     evals(OtherAtts,L) ++ " * " ++ evals(OtherAtts,R).
evals(OtherAtts,{Op,L,R}) ->
    "(" ++ atom_to_list(Op) ++ " " ++ evals(OtherAtts,L) ++ " " ++ evals(OtherAtts,R) ++ ")".

%% evaluation of eval receive predicate
evalr(OtherAtts,{parenthesis,P},M) ->
    "(" ++ evalr(OtherAtts,P,M) ++ ")";
evalr(OtherAtts,{bracket,L},M) ->
    "[" ++ string:join([evalr(OtherAtts,E,M) || E <- L], ",") ++ "]";
evalr(OtherAtts,{bracket2,L}, M) ->
    "sets:from_list([" ++ string:join([evalr(OtherAtts,E,M) || E <- L], ",") ++ "])";
evalr(OtherAtts,{head,N},M) -> evalr(OtherAtts,N,M) ++ ".head";
evalr(OtherAtts,{tail,N},M) -> evalr(OtherAtts,N,M) ++ ".tail";

%% NEED TO REVIEW
evalr(_,"true", _) -> "#t";
evalr(_,"false", _) -> "#f";
evalr(_,{self,Att},_) ->
    %"(this " ++ Att ++ ")";
    Att;
evalr(_,{param,Att},_) -> Att;
evalr(OtherAtts,{const,C},_) -> C;
evalr(OtherAtts,{minusconst,C}, _) -> "-" ++ C;
evalr(OtherAtts,[],_) -> "[]";
evalr(OtherAtts,{token,T},_) ->
    lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evalr(OtherAtts,[{self,Att}=Term], M) ->
    "[" ++ evalr(OtherAtts,Term, M) ++ "]";
evalr(OtherAtts,[H|T]=List,M) when T =/= [] ->
    S = [evalr(OtherAtts,Name,M) || Name <- List],
    "[" ++ string:join(S,",") ++ "]";
evalr(OtherAtts,{notmember, L, R}, M) ->
    L1= evalr(OtherAtts,L, M),
    R1 = evalr(OtherAtts,R,M),
    "not sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evalr(OtherAtts,{ismember, L, R},M) ->
    L1= evalr(OtherAtts,L,M),
    R1 = evalr(OtherAtts,R,M),
    "sets:is_element(" ++ L1 ++ "," ++ R1 ++ ")";
evalr(OtherAtts,{'not', T},M) ->
    " not " ++ evalr(OtherAtts,T,M);
evalr(OtherAtts,{literal,Name}, M) ->
    %io:format("verify ~p in ~p ~n",[Name,OtherAtts]),
    case lists:member(Name,OtherAtts) of
	true ->
	    "(that " ++ Name ++ ")";
	false ->
	    case proplists:get_value(Name,M) of
		undefined ->
		    lists:flatten(io_lib:format("~p",[list_to_atom(Name)]));
		    %"_" ++  string:uppercase(Name);
		I ->
		    "\"msg[" ++ integer_to_list(I) ++ "]\""
	    end
    end;
evalr(OtherAtts,{func,Name,L},_M) -> "user_code:" ++ Name ++ "(" ++ string:join([evalr(OtherAtts,C,_M) || C <- L],",") ++ ")";
evalr(OtherAtts,{Op,L,R},_M) ->
    "(" ++ atom_to_list(Op) ++ " " ++ evalr(OtherAtts,L,_M) ++ " " ++ evalr(OtherAtts,R,_M) ++ ")".


build_msg(Atts, Exps) ->
    build_msg(Atts, Exps,[]).

build_msg(_,empty,S) ->
    "()";
build_msg(_,[],S) -> %
    Res = string:join(lists:reverse(S)," "),
    Msg = "(" ++ if length(S) == 1 -> Res; true -> "list " ++ Res end ++ ")",
    Msg;
build_msg(Atts,[H|T], S) ->
    build_msg(Atts,T,[evale(Atts,H)|S]).

%% TODO: UMC does not support sending compound expressions
%% Thus need to represent them as temporal variables

evale(Atts,{head,Name}) -> evale(Atts,Name) ++ ".head";
evale(Atts,{tail,Name}) -> evale(Atts,Name) ++ ".tail";
evale(_,{self,Att}) ->
    %%"(this "++ Att ++ ")";
    Att;
evale(Atts,{literal,T}) ->
    case lists:member(T,Atts) of
	true ->
	    %"(this "++ T ++ ")";
	    T;
	false ->
	    lists:flatten(io_lib:format("~p",[list_to_atom(T)]))
    end;
%    "_" ++ string:uppercase(T);
evale(_,{token,T}) -> lists:flatten(io_lib:format("~p",[list_to_atom(T)]));
evale(_,{const,C}) -> C;
evale(_,{minusconst,C}) -> "(- 0 " ++ C ++ ")";
evale(Atts,{func,Name,L}) -> "user_code:" ++ Name ++ "(" ++ string:join([evale(Atts,C) || C <- L],",") ++ ")";
evale(Atts,{selector, L, I})  ->
    {L1,I1,C} = flatten(L,I),
    "user_code:selector" ++ integer_to_list(C) ++ "(" ++ evale(Atts,L1) ++ "," ++ string:join([evale(Atts,E) || E <- I1],",") ++ ")";
evale(Atts,{Op,L,R}) -> "(" ++ atom_to_list(Op) ++ " " ++ evale(Atts,L) ++ " " ++ evale(Atts,R) ++ ")".



%% helper function
flatten(L,I) -> flatten(L,[I],0).

flatten({selector,L,I1},I,C) ->
    flatten(L, [I1|I], C + 1);
flatten(L,I,C) ->
    {L,I,C+1}. % includes selector of the caller
