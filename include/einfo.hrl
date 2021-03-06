
-type type() :: atom().
-type msg() :: string().
-type line() :: non_neg_integer().
-type error() :: {error, einfo()} | {error, atom()} | undefined.
-type extra() :: any().

-record(einfo, {
          %% what would go on the second element in the error tuple
          %% {error, <type>}
          type :: type(),
          %% a human readable explanation of the error
          msg :: msg(),
          %% the module where this error was generated, can be filled using
          %% macros or parse transforms
          module :: module(),
          %% the function where this error was generated, can be filled using
          %% macros or parse transforms
          function :: function() | undefined,
          %% the function arity where this error was generated, can be filled
          %% using macros or parse transforms
          arity :: arity() | undefined,
          %% the line where this error was generated, can be filled using
          %% macros or parse transforms
          line :: line(),

          %% if you catch an error and generate another one, you can place
          %% the cause of this error in the cause field
          cause :: error(),
          %% a field where you can place extra context about the error that
          %% doesn't fit in other fields
          extra :: extra()}).

-type einfo() :: #einfo{}.

-ifdef(FUNCTION_NAME).
    -define(FUNCTION_NAME_SUPPORTED, true).
-else.
    -define(FUNCTION_NAME, undefined).
    -define(FUNCTION_NAME_SUPPORTED, false).
-endif.

-ifdef(FUNCTION_ARITY).
    -define(FUNCTION_ARITY_SUPPORTED, true).
-else.
    -define(FUNCTION_ARITY, undefined).
    -define(FUNCTION_ARITY_SUPPORTED, false).
-endif.

-define(NEW_ERROR(Type),
        {error, #einfo{type=Type, msg=atom_to_list(Type),
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       module=?MODULE, line=?LINE}}).
-define(NEW_ERROR(Type, Msg),
        {error, #einfo{type=Type, msg=Msg,
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       module=?MODULE, line=?LINE}}).
-define(NEW_ERROR(Type, Msg, Extra),
        {error, #einfo{type=Type, msg=Msg, module=?MODULE, line=?LINE,
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       extra=Extra}}).

-define(WRAP_ERROR(Type, Cause),
        {error, #einfo{type=Type, msg=atom_to_list(Type),
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       cause=Cause, module=?MODULE, line=?LINE}}).
-define(WRAP_ERROR(Type, Msg, Cause),
        {error, #einfo{type=Type, msg=Msg,
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       cause=Cause, module=?MODULE, line=?LINE}}).
-define(WRAP_ERROR(Type, Msg, Extra, Cause),
        {error, #einfo{type=Type, msg=Msg, extra=Extra,
                       function=?FUNCTION_NAME, arity=?FUNCTION_ARITY,
                       cause=Cause, module=?MODULE, line=?LINE}}).
