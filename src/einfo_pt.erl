-module(einfo_pt).
-export([parse_transform/2, format_error/1]).

-record(state, {module, function=undefined, function_line=-1, function_arity=-1,
				first_function=true, has_include_einfo=false}).

parse_transform(Forms0, _Options) ->
    State = #state{},
    {Forms, _NewState} = ast_walk:forms(Forms0, fun walker/2, State),
    Forms.

format_error(Error) -> atom_to_list(Error).

%% private

walker(State, Ast={attribute, _Line, include_lib, IncludePath}) ->
    case string:str(IncludePath, "/einfo.hrl") of
        0 -> {Ast, State};
        _ -> {Ast, State#state{has_include_einfo=true}}
    end;
walker(State=#state{first_function=true, has_include_einfo=false},
       Ast={function, Line, _Name, _Arity, _Clauses}) ->
    {[{attribute, Line, include_lib, "einfo/include/einfo.hrl"}, Ast],
	 State#state{first_function=false}};

walker(State, {pre, Ast={function, Line, Name, Arity, _Clauses}}) ->
    {Ast, State#state{function=Name, function_line=Line, function_arity=Arity}};

% einfo:error(Type)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, error}},
               [{atom,_,Type}]}) ->
    {error_record(State, Line, Type),
     State};
% einfo:error(Type, Reason)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, error}},
               [{atom,_,Type}, Reason]}) ->
    {error_record(State, Line, Type, Reason), State};
% einfo:error(Type, Reason, Extra)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, error}},
               [{atom,_,Type}, Reason, Extra]}) ->
    {error_record(State, Line, Type, Reason, Extra), State};

% einfo:wrap(Type, Cause)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, wrap}},
               [{atom,_,Type}, Cause]}) ->
    {wrap_error_record(State, Line, Type, Cause), State};
% einfo:wrap(Type, Reason, Cause)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, wrap}},
               [{atom,_,Type}, Reason, Cause]}) ->
    {wrap_error_record(State, Line, Type, Reason, Cause), State};
% einfo:wrap(Type, Reason, Extra, Cause)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, wrap}},
               [{atom,_,Type}, Reason, Extra, Cause]}) ->
    {error_record(State, Line, Type, Reason, Extra, Cause), State};

% einfo:format(Type, Format, FormatData)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, format}},
               [{atom,_,Type}, Format, FormatData]}) ->
	FormatCall = io_lib_format_ast(Line, Format, FormatData),
    {error_record(State, Line, Type, FormatCall), State};
% einfo:format(Type, Format, FormatData, Extra)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, format}},
               [{atom,_,Type}, Format, FormatData, Extra]}) ->
	FormatCall = io_lib_format_ast(Line, Format, FormatData),
    {error_record(State, Line, Type, FormatCall, Extra), State};

% einfo:wrap_format(Type, Format, FormatData, Cause)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, wrap_format}},
               [{atom,_,Type}, Format, FormatData, Cause]}) ->
	FormatCall = io_lib_format_ast(Line, Format, FormatData),
    {wrap_error_record(State, Line, Type, FormatCall, Cause), State};
% einfo:wrap_format(Type, Format, FormatData, Extra, Cause)
walker(State, {call, Line, {remote, _, {atom, _, einfo}, {atom, _, wrap_format}},
               [{atom,_,Type}, Format, FormatData, Extra, Cause]}) ->
	FormatCall = io_lib_format_ast(Line, Format, FormatData),
    {error_record(State, Line, Type, FormatCall, Extra, Cause), State};

walker(State, Ast={attribute, _Line, module, Module}) ->
    {Ast, State#state{module=Module}};
walker(State, Other) -> {Other, State}.

ast_undefined(Line) -> {atom, Line, nil}.

io_lib_format_ast(Line,  FormatAst,  FormatDataAst) ->
	{call, Line,
	 {remote, Line, {atom, Line, io_lib}, {atom, Line, format}},
	 [FormatAst, FormatDataAst]}.

wrap_error_record(State, Line, Type, Cause) ->
    wrap_error_record(State, Line, Type, {string, Line, atom_to_list(Type)},
                                          Cause).

wrap_error_record(State, Line, Type, Reason, Cause) ->
    error_record(State, Line, Type, Reason, ast_undefined(Line), Cause).

error_record(State, Line, Type) ->
    error_record(State, Line, Type, {string, Line, atom_to_list(Type)}).

error_record(State, Line, Type, Reason) ->
    error_record(State, Line, Type, Reason, ast_undefined(Line)).

error_record(State, Line, Type, Reason, Extra) ->
    error_record(State, Line, Type, Reason, Extra, ast_undefined(Line)).

error_record(#state{module=Module, function=Function, function_arity=Arity},
             Line, Type, Reason, Extra, Cause) ->
    {tuple, Line,
     [{atom, Line, error},
      {record, Line, einfo,
       [{record_field, Line, {atom, Line, type}, {atom, Line, Type}},
        {record_field, Line, {atom, Line, reason},  Reason},
        {record_field, Line, {atom, Line, module}, {atom, Line, Module}},
        {record_field, Line, {atom, Line, function}, {atom, Line, Function}},
        {record_field, Line, {atom, Line, arity}, {integer, Line, Arity}},
        {record_field, Line, {atom, Line, line}, {integer, Line, Line}},
        {record_field, Line, {atom, Line, cause}, Cause},
        {record_field, Line, {atom, Line, extra}, Extra}]}]}.
