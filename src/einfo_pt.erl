-module(einfo_pt).
-export([parse_transform/2, format_error/1]).

-record(state, {module, function, function_line=-1, function_arity=-1}).

parse_transform(Forms0, _Options) ->
    io:format("forms before: ~p~n", [Forms0]),
    State = #state{},
    {Forms, _NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    io:format("forms after: ~p~n", [Forms]),
    Forms.

format_error(Error) -> atom_to_list(Error).

%% private

walker(State, Ast={function, Line, Name, Arity, _Clauses}) ->
    {Ast, State#state{function=Name, function_line=Line, function_arity=Arity}};

walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, error}},
               [{atom,_,Type}]}) ->
    {error_record(State, Line, Type, {string, Line, atom_to_list(Type)}), State};
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, error}},
               [{atom,_,Type}, Reason]}) ->
    {error_record(State, Line, Type, Reason), State};

walker(State, Ast={attribute, _Line, module, Module}) ->
    {Ast, State#state{module=Module}};
walker(State, Other) -> {Other, State}.

error_record(#state{module=Module, function=Function, function_arity=Arity},
             Line, Type, Reason) ->
    {tuple, Line,
     [{atom, Line, error},
      {record, Line, einfo,
       [{record_field, Line, {atom, Line, type}, {atom, Line, Type}},
        {record_field, Line, {atom, Line, reason},  Reason},
        {record_field, Line, {atom, Line, module}, {atom, Line, Module}},
        {record_field, Line, {atom, Line, function}, {atom, Line, Function}},
        {record_field, Line, {atom, Line, arity}, {integer, Line, Arity}},
        {record_field, Line, {atom, Line, line}, {integer, Line, Line}}]}]}.
