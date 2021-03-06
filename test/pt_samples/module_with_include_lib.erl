-module(module_with_include_lib).
-export([f1/0, f2/1, f3/2, f_extra/1, wrap/0, wrap/1, wrap_msg/0,
        wrap_extra/0,
		format/1, format_extra/1,
		wrap_format/1, wrap_format_extra/1]).

-include_lib("einfo/include/einfo.hrl").

f1 () ->
    A = einfo:error(my_bad),
    f2(A).

f2(1) -> one;
f2(2) -> two;
f2(X) -> einfo:error(badarg, io_lib:format("bad argument: ~p", [X])).

f3(A, 0) ->
    einfo:error(division_by_zero, "dividing " ++ integer_to_list(A) ++ " by 0");
f3(A, B) -> A / B.

f_extra(A) ->
    einfo:error(badarg, io_lib:format("bad argument: ~p", [A]),
                [{arg, A}, {bad, true}]).

wrap() ->
    einfo:wrap(badarg, {error, parent_cause}).

wrap(Error) ->
    einfo:wrap(badarg, Error).

wrap_msg() ->
    einfo:wrap(badarg, "Msg", {error, parent_cause}).

wrap_extra() ->
    einfo:wrap(badarg, "Msg", [{with_parent, true}], {error, parent_cause}).

format(X) ->
	einfo:format(badarg, "bad argument: ~p", [X]).

format_extra(A) ->
    einfo:format(badarg, "bad argument: ~p", [A], [{arg, A}, {bad, true}]).

wrap_format(X) ->
	einfo:wrap_format(badarg, "bad argument: ~p", [X], {error, parent}).

wrap_format_extra(A) ->
    einfo:wrap_format(badarg, "bad argument: ~p", [A], [{arg, A}, {bad, true}],
	 {error, parent}).
