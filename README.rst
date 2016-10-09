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

	format(X) ->
		einfo:format(badarg, "bad argument: ~p", [X]).

	format_extra(A) ->
		einfo:format(badarg, "bad argument: ~p", [A], #{arg => A, bad => true}).

	wrap_format(X) ->
		einfo:wrap_format(badarg, "bad argument: ~p", [X], {error, parent}).

	wrap_format_extra(A) ->
		einfo:wrap_format(badarg, "bad argument: ~p", [A], #{arg => A, bad => true},
		 {error, parent}).

to:

.. code-block:: erl

	-module(module1).

	-include_lib("einfo/include/einfo.hrl").

	f1() ->
		A = {error,
		 #einfo{type = my_bad, reason = "my_bad",
			module = module1, function = f1, arity = 0, line = 8,
			cause = nil, extra = nil}},
		f2(A).

	f2(1) -> one;
	f2(2) -> two;
	f2(X) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = f2, arity = 1, line = 13,
			cause = nil, extra = nil}}.

	f3(A, 0) ->
		{error,
		 #einfo{type = division_by_zero,
			reason = "dividing " ++ integer_to_list(A) ++ " by 0",
			module = module1, function = f3, arity = 2, line = 16,
			cause = nil, extra = nil}};
	f3(A, B) -> A / B.

	f_extra(A) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [A]),
			module = module1, function = f_extra, arity = 1,
			line = 20, cause = nil,
			extra = #{arg => A, bad => true}}}.

	wrap() ->
		{error,
		 #einfo{type = badarg, reason = "badarg",
			module = module1, function = wrap, arity = 0, line = 24,
			cause = {error, parent_cause}, extra = nil}}.

	wrap(Error) ->
		{error,
		 #einfo{type = badarg, reason = "badarg",
			module = module1, function = wrap, arity = 1, line = 27,
			cause = Error, extra = nil}}.

	wrap_reason() ->
		{error,
		 #einfo{type = badarg, reason = "Reason",
			module = module1, function = wrap_reason, arity = 0,
			line = 30, cause = {error, parent_cause}, extra = nil}}.

	wrap_extra() ->
		{error,
		 #einfo{type = badarg, reason = "Reason",
			module = module1, function = wrap_extra, arity = 0,
			line = 33, cause = {error, parent_cause},
			extra = #{with_parent => true}}}.

	format(X) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = format, arity = 1,
			line = 36, cause = nil, extra = nil}}.

	format_extra(A) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [A]),
			module = module1, function = format_extra, arity = 1,
			line = 39, cause = nil,
			extra = #{arg => A, bad => true}}}.

	wrap_format(X) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = wrap_format, arity = 1,
			line = 42, cause = {error, parent}, extra = nil}}.

	wrap_format_extra(A) ->
		{error,
		 #einfo{type = badarg,
			reason = io_lib:format("bad argument: ~p", [A]),
			module = module1, function = wrap_format_extra,
			arity = 1, line = 45, cause = {error, parent},
			extra = #{arg => A, bad => true}}}.


Note that include_lib for einfo.hrl will only be included if it wasn't there

TODO
----

* fix ?FUNCTION_* macro detection
* test in a sample project

Ideas:

* maybe include only record definition instead of -include_lib einfo.hrl?
* remove macros and only use parse transform?
* einfo:to_string(EInfo)
* einfo:print(EInfo)

Author
------

Mariano Guerra

License
-------

BSD, see LICENSE
