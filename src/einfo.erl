-module(einfo).

%% API exports
-export([type/1, reason/1, module/1, line/1, function/1, arity/1]).
-export([function_name_supported/0, function_arity_supported/0]).

-include("einfo.hrl").

%%====================================================================
%% API functions
%%====================================================================

type(#einfo{type=Val}) -> Val.
reason(#einfo{reason=Val}) -> Val.
module(#einfo{module=Val}) -> Val.
line(#einfo{line=Val}) -> Val.
function(#einfo{function=Val}) -> Val.
arity(#einfo{arity=Val}) -> Val.

function_name_supported() -> ?FUNCTION_NAME_SUPPORTED.
function_arity_supported() -> ?FUNCTION_ARITY_SUPPORTED.

%%====================================================================
%% Internal functions
%%====================================================================
