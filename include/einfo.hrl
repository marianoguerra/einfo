
-record(einfo, {
          %% what would go on the second element in the error tuple
          %% {error, <type>}
          type,
          %% a human readable explanation of the error
          reason,
          %% the module where this error was generated, can be filled using
          %% macros or parse transforms
          module,
          %% the function where this error was generated, can be filled using
          %% macros or parse transforms
          function,
          %% the function arity where this error was generated, can be filled
          %% using macros or parse transforms
          arity,
          %% the line where this error was generated, can be filled using
          %% macros or parse transforms
          line,

          %% if you catch an error and generate another one, you can place
          %% the cause of this error in the cause field
          cause,
          %% a field where you can place extra context about the error that
          %% doesn't fit in other fields
          extra}).

-ifndef(FUNCTION_NAME).
    -define(FUNCTION_NAME, undefined).
    -define(FUNCTION_NAME_SUPPORTED, false).
-else.
    -define(FUNCTION_NAME_SUPPORTED, true).
-endif.

-ifndef(FUNCTION_ARITY).
    -define(FUNCTION_ARITY, undefined).
    -define(FUNCTION_ARITY_SUPPORTED, false).
-else.
    -define(FUNCTION_ARITY_SUPPORTED, true).
-endif.

-define(NEW_ERROR(Type),
        {error, #einfo{type=Type, reason=atom_to_list(Type),
                       module=?MODULE, line=?LINE}}).
-define(NEW_ERROR(Type, Reason),
        {error, #einfo{type=Type, reason=Reason, module=?MODULE, line=?LINE}}).
-define(NEW_ERROR(Type, Reason, Extra),
        {error, #einfo{type=Type, reason=Reason, module=?MODULE, line=?LINE,
                       extra=Extra}}).

-define(WRAP_ERROR(Type, Cause),
        {error, #einfo{type=Type, reason=atom_to_list(Type),
                       cause=Cause, module=?MODULE, line=?LINE}}).
-define(WRAP_ERROR(Type, Reason, Cause),
        {error, #einfo{type=Type, reason=Reason,
                       cause=Cause, module=?MODULE, line=?LINE}}).
-define(WRAP_ERROR(Type, Reason, Cause, Extra),
        {error, #einfo{type=Type, reason=Reason, extra=Extra,
                       cause=Cause, module=?MODULE, line=?LINE}}).
