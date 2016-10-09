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

API
---


Parameters
..........

Type
	An atom describing the type of error in a computer friendly way
	What you would put as second element in an error tuple: {error, Type}

Reason
	A string describint the error in human a friendly way

Extra
	Extra data that serves as context for the error, for example if the error
	was caused because of a bad key, you can add the key in the extra field

Cause
	If this error was caused by another internal error you can put the internal
	error in this field so you can have traceback-like information

Module
	The module where the error was generated as an atom

Function
	The function where the error was generated as an atom

Arity
	The arity of the function where the error was generated as an int

Line
	The line where the error was generated as an int

Parse Transforms
................

All calls set module, function, arity and line

einfo:error(Type)
	Create an error with type set, reason is a string version of type

einfo:error(Type, Reason)
	Create an error with type and reason set

einfo:error(Type, Reason, Extra)
	Create an error with type and reason and extra set

einfo:wrap(Type, Cause)
	Create an error with type and cause set, reason is a string version of type

einfo:wrap(Type, Reason, Cause)
	Create an error with type, reason and cause set

einfo:wrap(Type, Reason, Extra, Cause)
	Create an error with type, reason, extra and cause set

einfo:format(Type, Format, FormatData)
	Create an error with type set, reason is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:format(Type, Format, FormatData, Extra)
	Create an error with type and extra set,
	reason is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:wrap_format(Type, Format, FormatData, Cause)
	Create an error with type and cause set,
	reason is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:wrap_format(Type, Format, FormatData, Extra, Cause)
	Create an error with type, extra and cause set,
	reason is a the result of calling at runtime
	io_lib:format(Format, FormatData)

Functions
.........

einfo:to_string(EInfo | {error, EInfo})
	Return a one line string representation of the error, without the cause
	Something like:
	'Error: {type}\@{module}:{function}/{arity}:{line} \"{reason}\" ({extra})'

einfo:print(EInfo | {error, EInfo})
	print string representation with io:format

TODO
----

* fix ?FUNCTION_* macro detection
* test in a sample project

Ideas:

* maybe include only record definition instead of -include_lib einfo.hrl?
* remove macros and only use parse transform?
* add getter for extra parameter, make it work for proplists and maps

Author
------

Mariano Guerra

License
-------

BSD, see LICENSE
