einfo
=====

A library, macros and parse transform for structured, standard errors with more
context

Build
-----

::

    rebar3 compile

Test
----

::

    rebar3 ct

it will transform the module in test/pt_samples/module1.erl from:

.. code-block:: erl

    -module(module1).
    -export([f1/0, f2/1, f3/2, f_extra/1, wrap/0, wrap/1, wrap_reason/0,
            wrap_extra/0]).

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
                    #{arg => A, bad => true}).

    wrap() ->
        einfo:wrap(badarg, {error, parent_cause}).

    wrap(Error) ->
        einfo:wrap(badarg, Error).

    wrap_reason() ->
        einfo:wrap(badarg, "Reason", {error, parent_cause}).

    wrap_extra() ->
        einfo:wrap(badarg, "Reason", #{with_parent => true}, {error, parent_cause}).

to:

.. code-block:: erl

	-module(module1).

	-export([f1/0, f2/1, f3/2, f_extra/1, wrap/0, wrap/1,
			 wrap_reason/0, wrap_extra/0]).

	-include_lib("einfo/include/einfo.hrl").

	f1() ->
		A = {error,
			 #einfo{type = my_bad, reason = "my_bad",
					module = module1, function = f1, arity = 0, line = 6,
					cause = nil, extra = nil}},
		f2(A).

	f2(1) -> one;
	f2(2) -> two;
	f2(X) ->
		{error,
		 #einfo{type = badarg,
				reason = io_lib:format("bad argument: ~p", [X]),
				module = module1, function = f2, arity = 1, line = 11,
				cause = nil, extra = nil}}.

	f3(A, 0) ->
		{error,
		 #einfo{type = division_by_zero,
				reason = "dividing " ++ integer_to_list(A) ++ " by 0",
				module = module1, function = f3, arity = 2, line = 14,
				cause = nil, extra = nil}};
	f3(A, B) -> A / B.

	f_extra(A) ->
		{error,
		 #einfo{type = badarg,
				reason = io_lib:format("bad argument: ~p", [A]),
				module = module1, function = f_extra, arity = 1,
				line = 18, cause = nil,
				extra = #{arg => A, bad => true}}}.

	wrap() ->
		{error,
		 #einfo{type = badarg, reason = "badarg",
				module = module1, function = wrap, arity = 0, line = 22,
				cause = {error, parent_cause}, extra = nil}}.

	wrap(Error) ->
		{error,
		 #einfo{type = badarg, reason = "badarg",
				module = module1, function = wrap, arity = 1, line = 25,
				cause = Error, extra = nil}}.

	wrap_reason() ->
		{error,
		 #einfo{type = badarg, reason = "Reason",
				module = module1, function = wrap_reason, arity = 0,
				line = 28, cause = {error, parent_cause}, extra = nil}}.

	wrap_extra() ->
		{error,
		 #einfo{type = badarg, reason = "Reason",
				module = module1, function = wrap_extra, arity = 0,
				line = 31, cause = {error, parent_cause},
				extra = #{with_parent => true}}}.

Note that include_lib for einfo.hrl will only be included if it wasn't there

TODO
----

* fix ?FUNCTION_* macro detection
* test in a sample project

Ideas:

* maybe include only record definition instead of -include_lib einfo.hrl?
* remove macros and only use parse transform?
* einfo:throw_error/1,2,3
* einfo:throw_wrap/2,3,4
* einfo:error_format(Type, Fmt, Args) % Reason is io_lib:format(Fmt, Args)
* einfo:to_string(EInfo)
* einfo:print(EInfo)

Author
------

Mariano Guerra

License
-------

BSD, see LICENSE
