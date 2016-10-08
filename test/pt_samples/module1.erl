-module(module1).
-export([f1/0, f2/1, f3/2]).

f1 () ->
    A = einfo:error(my_bad),
    f2(A).

f2(1) -> one;
f2(2) -> two;
f2(X) -> einfo:error(badarg, io_lib:format("bad argument: ~p", [X])).

f3(A, 0) ->
    einfo:error(division_by_zero, "dividing " ++ integer_to_list(A) ++ " by 0");
f3(A, B) -> A / B.
