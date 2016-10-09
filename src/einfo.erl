-module(einfo).

%% API exports
-export([type/1, msg/1, module/1, line/1, function/1, arity/1, cause/1,
         extra/1, extra/2, extra/3]).
-export([function_name_supported/0, function_arity_supported/0]).
-export([to_string/1, print/1]).
-ifdef(supports_maps).
-export([to_map/1]).
-endif.
-export([to_plist/1]).

-include("einfo.hrl").

-define(FORMAT, "Error: ~p@~p:~p/~p:~p ~p (~p)").

%%====================================================================
%% Types
%%====================================================================

-type einfo() :: #einfo{}.
-type type() :: atom().
-type msg() :: string().
-type line() :: non_neg_integer().
-type error() :: {error, einfo()} | {error, atom()} | undefined.
-type extra() :: any().

%%====================================================================
%% API functions
%%====================================================================

-spec type(einfo()) -> type().
type(#einfo{type=Val}) -> Val.

-spec msg(einfo()) -> msg().
msg(#einfo{msg=Val}) -> Val.

-spec module(einfo()) -> module().
module(#einfo{module=Val}) -> Val.

-spec line(einfo()) -> line().
line(#einfo{line=Val}) -> Val.

-spec function(einfo()) -> function() | undefined.
function(#einfo{function=Val}) -> Val.

-spec arity(einfo()) -> arity() | undefined.
arity(#einfo{arity=Val}) -> Val.

-spec cause(einfo()) -> error().
cause(#einfo{cause=Val}) -> Val.

-spec extra(einfo()) -> extra().
extra(#einfo{extra=Val}) -> Val.

-spec extra(einfo(), any()) -> any().
extra(EInfo, Key) -> extra(EInfo, Key, undefined).

-ifdef(supports_maps).
-spec extra(einfo(), any(), any()) -> any().
extra(#einfo{extra=Extra}, Key, Default) when is_map(Extra) ->
    maps:get(Key, Extra, Default);
extra(#einfo{extra=Extra}, Key, Default) when is_list(Extra) ->
    proplists:get_value(Key, Extra, Default);
extra(#einfo{}, _Key, Default) ->
    Default.
-else.
-spec extra(einfo(), any(), any()) -> any().
extra(#einfo{extra=Extra}, Key, Default) when is_list(Extra) ->
    proplists:get_value(Key, Extra, Default);
extra(#einfo{}, _Key, Default) ->
    Default.
-endif.

function_name_supported() -> ?FUNCTION_NAME_SUPPORTED.
function_arity_supported() -> ?FUNCTION_ARITY_SUPPORTED.

-spec to_string(einfo()) -> string().
to_string(EInfo) ->
    io_lib:format(?FORMAT, to_string_args(EInfo)).

-spec print(einfo()) -> any().
print(EInfo) ->
    io:format(?FORMAT "~n", to_string_args(EInfo)).

-spec to_plist(einfo()) -> [proplists:property()].
to_plist(#einfo{type=Type, msg=Msg, module=Module, function=Fun, arity=Arity,
                line=Line, extra=Extra, cause=Cause}) ->
    [{type, Type}, {msg, Msg}, {module, Module}, {function, Fun}, 
     {arity, Arity}, {line, Line}, {extra, Extra}, {cause, Cause}].

-ifdef(supports_maps).
-spec to_map(einfo()) -> map().
to_map(#einfo{type=Type, msg=Msg, module=Module, function=Fun, arity=Arity,
                line=Line, extra=Extra, cause=Cause}) ->
    #{type => Type, msg => Msg, module => Module, function => Fun,
      arity => Arity, line => Line, extra => Extra, cause => Cause}.
-endif.

%%====================================================================
%% Internal functions
%%====================================================================

to_string_args({error, EInfo=#einfo{}}) ->
    to_string_args(EInfo);
to_string_args(#einfo{type=Type, msg=Msg, module=Module,
                      function=Fun, arity=Arity, line=Line, extra=Extra}) ->
    [Type, Module, Fun, Arity, Line, Msg, Extra].
