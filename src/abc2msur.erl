-module(abc2msur).
-export([file/1, main/1, view/1, flatten/1]).
-define(Config,"(#sync-1-n send receive)\n\n(#restrict receive)\n\n").
-define(FS,"  ").
-record(state,{aware, unfold, params, space}).

print(X) -> io:format("~p~n", [X]).

%% build the project
main(["build"]) ->
    leex:file(scanner),
    yecc:file(parser);
main([Fname]) -> file(Fname);
main(["scan", Fname]) ->
	{ok, Binary} = file:read_file(Fname),
	print(scanner:string(binary_to_list(Binary))).

%% run a file name
file([Atom]) when is_atom(Atom) ->
    file(atom_to_list(Atom) ++ ".abc");
file(Fname) ->
    {ok, Binary} = file:read_file(Fname),
    [Out|_] = string:split(Fname,"."),
    trans(Out, binary_to_list(Binary)).

%% For viewing the AST
view([Atom]) when is_atom(Atom) ->
    view(atom_to_list(Atom) ++ ".abc");
view(Fname) ->
    {ok, Binary} = file:read_file(Fname),
    view1(binary_to_list(Binary)).
view1(Code) ->
    {ok, Tree} = parser:scan_and_parse(Code),
    io:format("~p~n",[Tree]).

%% translate the code
trans(Fname, String) when is_list(String) ->
    {ok, Tree} = parser:scan_and_parse(String), % get AST

    %% extract necessary system structure
    Sys = [Y || {sys, Y} <- Tree], % Considering those components are declared under SYS
    CompDefs = [X || X <- Tree, element(1,X) == comp], % component definitions
    Comp_inits = [X || X <- Tree, element(1,X) == comp_init], % component instantiations
    CNames = [X || {comp, X, _} <- CompDefs], % component names

    catch ets:delete(abcsystem),
    catch ets:delete(counter),
    catch ets:delete(auxilary),
    catch ets:delete(msursystem),

    ets:new(counter,[named_table]),
    ets:new(auxilary,[named_table]),
    ets:new(abcsystem,[named_table]), %% the table that keeps all
    %% Store code definitions
    preprocessing(CompDefs), % FULL OF SIDE-EFFECTS!
    AttEnv = ets:lookup_element(abcsystem, attributes, 2),
    ets:new(msursystem,[named_table]), %% the table that store trasnlated code
    ets:insert(msursystem,{allmodules, CNames}),
    body(Fname, CNames, Comp_inits). % the main work

%% translate each component type!
body(Fname, [CName|Rest], Comp_inits) ->
    Header = ";; code for component type " ++ atom_to_list(CName) ++ "\n",
    ets:insert(CName,{body,[]}),
    ets:insert(CName, {header, Header}),

    Process = #{ proc_name => CName},
    ets:insert(CName, {visited, []}), % the init process have the same name as component name

    AllDefs = ets:lookup_element(CName,beh,2),

    [call(Def,CName) || Def <- AllDefs],

    CodeForCName = lists:reverse(ets:lookup_element(CName,body,2)),
    %io:format("Before normalization ~p ~n",[CodeForCName]),

    ets:insert(CName,{body,CodeForCName}),
    body(Fname, Rest, Comp_inits);
body(Fname, [], Comp_inits) ->
    %% Print = "\n %% COMPONENT BEHAVIOUR " ++ atom_to_list(CName) ++ "\n",
    %% output_abel(CName,Print),
    Vars = sets:to_list(sets:from_list(ets:lookup_element(abcsystem, vars, 2))),
    Attrs = sets:to_list(sets:from_list(lists:foldl(fun(X, Acc) ->
				X ++ Acc
			end, [], maps:values(ets:lookup_element(abcsystem, attributes, 2))))),
    RepVars = "(#replicated-vars " ++ string:join(lists:reverse(Vars ++ Attrs)," ") ++ ")\n\n",

    %% Collect attribute environments of all compnents
    {Comps, Sys}  = lists:foldl(fun({comp_init, CInit, {comp, Comp, Decl}}, {Acc, Sys}) ->
					Elem = "(#agent " ++ string(CInit) ++ " "
					    ++ string(Comp) ++ " "
					    ++  build_env(Decl) ++ " "
					    ++ string:join(lists:map(fun(X) -> "(" ++ X ++ " 0)" end, Vars), " ")
					    ++ ")",
					{[Elem | Acc],[CInit | Sys]}
				end, {[],[]}, Comp_inits),
    Comps2 = string:join(lists:reverse(Comps),"\n") ++ "\n\n",
    Sys2 = "(#system " ++ string:join(lists:map(fun(X) -> "(" ++ string(X) ++ " 1)" end, lists:reverse(Sys))," ") ++ ")\n\n",

    file:write_file(Fname ++ ".msur",[?Config,RepVars]),
    AllNames = ets:lookup_element(msursystem,allmodules,2),
    [file:write_file(Fname ++ ".msur", [ets:lookup_element(CName,header,2), ets:lookup_element(CName,body,2)], [append]) || CName <- AllNames],
    file:write_file(Fname ++ ".msur",[";; initialization \n\n",Comps2, Sys2], [append]).

%% Helper functions
call(Code, CName) ->
    %% carry out the information as how to produce code for an action
    ActionState = #state{aware = [], % no awareness yet
			 unfold = false, %true to unfold the choice
			 params = [], % formal params
			 space = 1 %spacing
			},
    eval(Code, CName, ActionState).

eval({proc,Args,Code}, CName, ActionState) ->
    eval(Code,CName, ActionState#state{params = Args});

eval({def, ProcName, Args, Code}, CName, #state{space = SP} = ActionState) ->
    %ignore Args for now
    R = "(#def " ++ atom_to_list(ProcName) ++ "\n" ++ space(SP) ++ eval(Code, CName, ActionState#state{params = Args, space = SP + 1}) ++ ")\n\n",
    print(CName,R);

eval({p_awareness, Pred, Process}, CName, ActionState) ->
    eval(Process, CName, ActionState#state{aware = Pred});

%% Prefixing process, con is a process call
eval({prefix, Left, nil}, CName, #state{space = SP} = ActionState) ->
    build_act(Left, CName, ActionState);

eval({prefix, Left, Right}, CName, #state{space = SP} = ActionState) ->
    space(SP) ++ "(#seq\n" ++
	build_act(Left, CName, ActionState#state{space = SP + 1}) ++ "\n" ++
	eval(Right, CName, ActionState#state{space = SP + 1}) ++
	")";

eval({call, ProcName, Args}, CName, ActionState = #state{space = SP, unfold = UF}) ->
    space(SP) ++ "(#call " ++ atom_to_list(ProcName) ++ ")";

eval({choice, _,_} = Code, CName, #state{space = SP} = ActionState) ->
    space(SP) ++  "(#choice\n" ++ string:join([eval(X, CName, ActionState#state{space = SP + 1}) || X <- flatten(Code)], " \n") ++ ")";

eval({par, _, _} = Code, CName, ActionState = #state{space = SP, params = Args}) ->
    space(SP) ++ "(#par\n" ++ string:join([eval(P, CName, ActionState#state{space = SP + 1}) || P <- flatten(Code)], " \n") ++ ")";

eval(nil, _, ActionState = #state{space = SP}) ->
    space(SP) ++ "nop";

%% should never get in here
eval(FINAL, CName, ActionState) ->
    io:format("FINAL DOES NOT MATCH, ~p ~p ~p ",[FINAL, CName, ActionState]).


%% get specifications of actions
build_act({{output, Exps, Pred}, Upd}, CName, #state{aware = Aware, params = Params, space = SP}) ->
    MyAtts = my_attrs(CName),
    %io:format("my att ~p~n",[MyAtts]),
    OtherAtts = other_attrs(CName),
    G = utils:build_apred(MyAtts,Aware),
    M = utils:build_msg(MyAtts,Exps),
    U = utils:build_update(MyAtts,Upd,[]),
    SPred = utils:build_spred(OtherAtts,Pred),
    Act = "(send " ++ string:join([SPred,M,U]," ") ++ ")",
    Res = if length(G) == 0 -> Act;
	     true -> "(#guard " ++ G ++ "\n" ++ space(SP+1) ++ Act ++ ")"
	  end,
    space(SP) ++ Res;

build_act({{input, Pred, Vars}, Upd}, CName, #state{aware = Aware, params = Params, space = SP}) ->
    collect_vars(Vars),
    MyAtts = my_attrs(CName),
    OtherAtts = other_attrs(CName),
    Msg = bound_assignment(Vars),
    X = "(" ++ if length(Vars) == 1 ->
		Vars;
		  true -> "list " ++ string:join(Vars," ")
	       end
	++ ")",
    G = utils:build_apred(MyAtts,Aware),
    U = utils:build_update(MyAtts,Upd,Msg),
    %io:format("My Atts ~p, Other Atts ~p~n",[MyAtts,OtherAtts]),
    RP = utils:build_rpred(OtherAtts,Pred,Msg),
    Act = "(receive " ++ string:join([RP,X,U]," ") ++ ")",
    Res = if length(G) == 0 -> Act;
	     true -> "(#guard " ++ G ++ "\n" ++ space(SP+1) ++ Act ++ ")"
	  end,
   space(SP) ++ Res.


preprocessing(CompDefs) ->
    ets:insert(abcsystem, {attributes, #{}}),
    ets:insert(abcsystem, {vars, []}),
    %% component indexes
    I = lists:seq(0,length(CompDefs) - 1),
    Comps = lists:zip(I, CompDefs),
    %% storing components behaviour definitions and initializing them into each own table
    lists:map(fun({I1, {comp, CName, [{attrs, AttList}, {beh, Behaviour, {_,Init}}]}}) -> % FULL OF SIDE-EFFECT
		      %% each component has an information table
		      catch ets:delete(CName),
		      ets:new(CName, [named_table]),
		      %% store the init behaviour of component definition CName
		      InitBeh = {def, CName, [], Init},
		      %% now store also all beh
		      ets:insert(CName, {beh, Behaviour ++ [InitBeh]}),

		      %% store process definitions of this component
		      Fun = fun({def, ProcName, Args, Code}) ->
				    ets:insert(CName, {ProcName, {proc, Args, Code}})
				    %ets:insert(CName, {ProcName, {def, ProcName, Args, Code}})
			    end,
		      lists:map(Fun, Behaviour),
		      %% Create the attributes list from component definitions
		      %% Assigning component indexes to each attribute name
		      Acc = ets:lookup_element(abcsystem, attributes, 2),
		      Map = maps:put(CName, AttList, Acc),
		      ets:insert(abcsystem, {attributes, Map})
	      end, Comps).
    %io:format("Set of attribute collected is ~p~n",[ets:lookup_element(abcsystem, attributes, 2)]),


build_env(Data) ->
    string:join(
      lists:map(fun({K,V}) ->
			"(" ++ K ++ " " ++ data_eval(V) ++ ")"
		end,
		Data),
      " ").

data_eval({token,T}) ->
    L = get(token),
    put(token, L#{T => T}),
    T;
data_eval({const,C}) ->
    C;
data_eval({minusconst,C}) ->
    "-" ++ C;
data_eval(empty_vector) ->
    "[]";
% an array of single elements
data_eval({bracket, List}) when is_list(List) ->
   % io:format("eval for ~p ~n",[List]),
    Array = [data_eval(X) || X <- List],
    "[" ++ string:join(Array,",") ++ "]";
data_eval({bracket, E}) ->
    "[" ++ data_eval(E) ++ "]";
data_eval(Other) -> Other.

filter_bindings(Args, VarNames) -> % vars is the message, bindings is
    %% simplest case, Vars \in Bindings
    Name = [X || {_,X} <- Args],
    L = [{X,proplists:get_value(X,VarNames)} || X <- Name].

select_bindings(FormalArgs, ActualArgs) ->
    Vals = [I || {_,I} <- ActualArgs],
    lists:zip(FormalArgs,Vals).

%% previous vars [{x,0}, {y,1}, ...]
%% new vars  = [x, z,t,w],
%% get several things: assign variable in the message to bound, return new proplist of [{allvars , indexes}] and the message
bound_assignment(Vars) ->
%    io:format("previous vars empty ~n",[]),
    Index1 = lists:seq(0, length(Vars)-1),
    Msg = lists:zip(Vars, Index1),
    Msg.


output_abel(Name,Code) ->
    Body = ets:lookup_element(Name,body,2),
    ets:insert(Name, {body, [Code|Body]}).


string(T) ->
    %lists:flatten(io_lib:format("~p",[list_to_atom(string:lowercase(atom_to_list(T)))])).
    atom_to_list(T).



%%% choice choice choice
flatten({OP, L, R})->
    flatten({OP, L, R},[]).

flatten({OP, {OP,_,_}=L, R}, Acc) ->
    flatten(L,[R | Acc]);
flatten({OP,L,R},Acc) ->
    lists:flatten([L,R | Acc]).


print(CName,R) ->
    output_abel(CName,R),
    ok.
    %io:format("R = ~p~n",[R]).


other_attrs(CName) ->
    %% other attributes should be all attributes
    Attrs = ets:lookup_element(abcsystem, attributes, 2),
    Others = lists:append([X || {O,X} <- maps:to_list(Attrs)]).

my_attrs(CName) ->
    Attrs = ets:lookup_element(abcsystem, attributes, 2),
    My = lists:append([X || {O,X} <- maps:to_list(Attrs), O == CName]).

space(C) ->
    string:copies(?FS,C).

collect_vars(Vars) ->
    Acc = ets:lookup_element(abcsystem, vars, 2),
    ets:insert(abcsystem, {vars, Vars ++ Acc}).
