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

to:

.. code-block:: erl

	-module(module1).

	-export([f1/0, f2/1, f3/2]).

	f1() ->
		A = {error,
			 #einfo{type = my_bad, reason = "my_bad",
					module = module1, function = f1, arity = 0, line = 5}},
		f2(A).

	f2(1) -> one;
	f2(2) -> two;
	f2(X) ->
		{error,
		 #einfo{type = badarg,
				reason = io_lib:format("bad argument: ~p", [X]),
				module = module1, function = f2, arity = 1, line = 10}}.

	f3(A, 0) ->
		{error,
		 #einfo{type = division_by_zero,
				reason = "dividing " ++ integer_to_list(A) ++ " by 0",
				module = module1, function = f3, arity = 2, line = 13}};
	f3(A, B) -> A / B.

TODO
----

* include -include_lib einfo attribute if not there
* fix ?FUNCTION_* macro detection

Ideas:

* remove macros and only use parse transform?
* einfo:throw_error/1,2,3
* einfo:throw_wrap/2,3,4
* einfo:error(Type, Reason, Extra) % extra is a proplist or map
* einfo:error_format(Type, Fmt, Args) % Reason is io_lib:format(Fmt, Args)
* einfo:wrap(Type, Cause), wrap(Type, Reason, Cause), wrap(Type, Reason, Cause, Extra)
* einfo:to_string(EInfo)
* einfo:print(EInfo)

Author
------

Mariano Guerra

License
-------

BSD, see LICENSE
