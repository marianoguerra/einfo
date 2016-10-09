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

Use
---

Add einfo as a dependency on your project, on rebar for example:

.. code-block:: erl

    {einfo, {git, "https://github.com/marianoguerra/einfo", {branch, "master"}}}

Add the einfo_pt parse transform to your config, on rebar for example:

.. code-block:: erl

	{erl_opts, [debug_info, {parse_transform, einfo_pt}]}.

For now, on the module you want to use it, include einfo.hrl:

.. code-block:: erl

	-include_lib("einfo/include/einfo.hrl").

What does it do
---------------

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

	wrap_msg() ->
		einfo:wrap(badarg, "Msg", {error, parent_cause}).

	wrap_extra() ->
		einfo:wrap(badarg, "Msg", #{with_parent => true}, {error, parent_cause}).

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
		 #einfo{type = my_bad, msg = "my_bad",
			module = module1, function = f1, arity = 0, line = 8,
			cause = undefined, extra = undefined}},
		f2(A).

	f2(1) -> one;
	f2(2) -> two;
	f2(X) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = f2, arity = 1, line = 13,
			cause = undefined, extra = undefined}}.

	f3(A, 0) ->
		{error,
		 #einfo{type = division_by_zero,
			msg = "dividing " ++ integer_to_list(A) ++ " by 0",
			module = module1, function = f3, arity = 2, line = 16,
			cause = undefined, extra = undefined}};
	f3(A, B) -> A / B.

	f_extra(A) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [A]),
			module = module1, function = f_extra, arity = 1,
			line = 20, cause = undefined,
			extra = #{arg => A, bad => true}}}.

	wrap() ->
		{error,
		 #einfo{type = badarg, msg = "badarg",
			module = module1, function = wrap, arity = 0, line = 24,
			cause = {error, parent_cause}, extra = undefined}}.

	wrap(Error) ->
		{error,
		 #einfo{type = badarg, msg = "badarg",
			module = module1, function = wrap, arity = 1, line = 27,
			cause = Error, extra = undefined}}.

	wrap_msg() ->
		{error,
		 #einfo{type = badarg, msg = "Msg",
			module = module1, function = wrap_msg, arity = 0,
			line = 30, cause = {error, parent_cause}, extra = undefined}}.

	wrap_extra() ->
		{error,
		 #einfo{type = badarg, msg = "Msg",
			module = module1, function = wrap_extra, arity = 0,
			line = 33, cause = {error, parent_cause},
			extra = #{with_parent => true}}}.

	format(X) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = format, arity = 1,
			line = 36, cause = undefined, extra = undefined}}.

	format_extra(A) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [A]),
			module = module1, function = format_extra, arity = 1,
			line = 39, cause = undefined,
			extra = #{arg => A, bad => true}}}.

	wrap_format(X) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [X]),
			module = module1, function = wrap_format, arity = 1,
			line = 42, cause = {error, parent}, extra = undefined}}.

	wrap_format_extra(A) ->
		{error,
		 #einfo{type = badarg,
			msg = io_lib:format("bad argument: ~p", [A]),
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

Msg
	A string describing the error in human a friendly way

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
	Create an error with type set, msg is a string version of type

einfo:error(Type, Msg)
	Create an error with type and msg set

einfo:error(Type, Msg, Extra)
	Create an error with type and msg and extra set

einfo:wrap(Type, Cause)
	Create an error with type and cause set, msg is a string version of type

einfo:wrap(Type, Msg, Cause)
	Create an error with type, msg and cause set

einfo:wrap(Type, Msg, Extra, Cause)
	Create an error with type, msg, extra and cause set

einfo:format(Type, Format, FormatData)
	Create an error with type set, msg is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:format(Type, Format, FormatData, Extra)
	Create an error with type and extra set,
	msg is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:wrap_format(Type, Format, FormatData, Cause)
	Create an error with type and cause set,
	msg is a the result of calling at runtime
	io_lib:format(Format, FormatData)

einfo:wrap_format(Type, Format, FormatData, Extra, Cause)
	Create an error with type, extra and cause set,
	msg is a the result of calling at runtime
	io_lib:format(Format, FormatData)

Functions
.........

einfo:to_string(EInfo | {error, EInfo})
	Return a one line string representation of the error, without the cause
	Something like:
	'Error: {type}\@{module}:{function}/{arity}:{line} \"{msg}\" ({extra})'

einfo:print(EInfo | {error, EInfo})
	print string representation with io:format

type(EInfo)
    Return the value of the type field
msg(EInfo)
    Return the value of the msg field
module(EInfo)
    Return the value of the module field
line(EInfo)
    Return the value of the line field
function(EInfo)
    Return the value of the function field
arity(EInfo)
    Return the value of the arity field
cause(EInfo)
    Return the value of the cause field
extra(EInfo)
    Return the value of the extra field
extra(EInfo, Key)
    Lookup Key in the extra field, works if Extra is a Map or PropList, returns
    undefined if not found
extra(EInfo, Key, Default)
    Lookup Key in the extra field, works if Extra is a Map or PropList, returns
    Default if not found

to_plist(EInfo)
    Returns the EInfo record as a proplist

to_map(EInfo)
    Returns the EInfo record as a map, supported on Erlang >= 17

Macros
......

Note: **function** and **arity** fields will only be set on Erlang >= 19

NEW_ERROR(Type)
	Like einfo:error/1 but as a macro
NEW_ERROR(Type, Msg)
	Like einfo:error/2 but as a macro
NEW_ERROR(Type, Msg, Extra)
	Like einfo:error/3 but as a macro

WRAP_ERROR(Type, Cause)
	Like einfo:wrap/2 but as a macro
WRAP_ERROR(Type, Msg, Cause)
	Like einfo:wrap/3 but as a macro
WRAP_ERROR(Type, Msg, Extra, Cause)
	Like einfo:wrap/4 but as a macro

TODO
----

* automatic include_lib doesn't seem to be working

Ideas:

* maybe include only record definition instead of -include_lib einfo.hrl?

Author
------

Mariano Guerra

License
-------

BSD, see LICENSE
