-module(einfo).

%% API exports
-export([type/1, reason/1, module/1, line/1, function/1, arity/1]).
-export([function_name_supported/0, function_arity_supported/0]).
-export([to_string/1, print/1]).

-include("einfo.hrl").

-define(FORMAT, "Error: ~p@~p:~p/~p:~p ~p (~p)").

%%====================================================================
%% API functions
%%====================================================================

type(#einfo{type=Val}) -> Val.
reason(#einfo{reason=Val}) -> Val.
module(#einfo{module=Val}) -> Val.
line(#einfo{line=Val}) -> Val.
function(#einfo{function=Val}) -> Val.
arity(#einfo{arity=Val}) -> Val.

function_name_supported() -> ?FUNCTION_NAME_SUPPORTED.
function_arity_supported() -> ?FUNCTION_ARITY_SUPPORTED.

to_string(EInfo) ->
    io_lib:format(?FORMAT, to_string_args(EInfo)).

print(EInfo) ->
    io:format(?FORMAT, to_string_args(EInfo)).

%%====================================================================
%% Internal functions
%%====================================================================

to_string_args({error, EInfo=#einfo{}}) ->
    to_string_args(EInfo);
to_string_args(#einfo{type=Type, reason=Reason, module=Module,
                      function=Fun, arity=Arity, line=Line, extra=Extra}) ->
    [Type, Module, Fun, Arity, Line, Reason, Extra].
