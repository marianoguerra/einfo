-module(einfo_pt_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [module1].

module1(_) ->
    Path = filename:absname("../../../../test/pt_samples/module1.erl"),
    {ok, Ast} = epp:parse_file(Path, [], []),
    Result = einfo_pt:parse_transform(Ast, nil),
    Code = erl_prettypr:format(erl_syntax:form_list(Result)),
    file:write_file("../../../../out.txt", list_to_binary(Code)),
    true = string:str(Code, "#einfo{") > 0.
