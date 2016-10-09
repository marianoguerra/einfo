-module(einfo_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("einfo.hrl").





error_at_line_11(Type) -> ?NEW_ERROR(Type).
error_at_line_12(Type, Reason) -> ?NEW_ERROR(Type, Reason).

all() -> [error_here_1, error_here_2, to_string].

error_here_1(_) ->
    {error, EInfo} = error_at_line_11(my_error),
    my_error = einfo:type(EInfo),
    "my_error" = einfo:reason(EInfo),
    11 = einfo:line(EInfo),
    einfo_SUITE = einfo:module(EInfo),
    ct:pal("~p ~p", [?FUNCTION_NAME, ?FUNCTION_ARITY]),
    case einfo:function_name_supported() of
        true ->
            error_at_line_11 = einfo:function(EInfo),
            1 = einfo:arity(EInfo);
        false ->
            undefined = einfo:function(EInfo),
            undefined = einfo:arity(EInfo)
    end.

error_here_2(_) ->
    {error, EInfo} = error_at_line_12(my_error, "My Reason"),
    my_error = einfo:type(EInfo),
    "My Reason" = einfo:reason(EInfo),
    12 = einfo:line(EInfo).

to_string(_) ->
    EInfo = #einfo{type = my_error,
                   reason = "Reason",
                   module = module1, function = format, arity = 1,
                   line = 39, cause = nil,
                   extra = #{extra => true}},
    Error = {error, EInfo},
    Expect = "Error: my_error@module1:format/1:39 \"Reason\" (#{extra => true})",
    String1 = lists:flatten(einfo:to_string(Error)),
    String2 = lists:flatten(einfo:to_string(EInfo)),
    Expect = String1,
    Expect = String2.
