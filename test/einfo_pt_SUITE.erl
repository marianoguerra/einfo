-module(einfo_pt_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [module1, module_with_include_lib].

module1(_) ->
    Path = filename:absname("../../../../test/pt_samples/module1.erl"),
    {ok, Ast} = epp:parse_file(Path, [], []),
    Result = einfo_pt:parse_transform(Ast, nil),
    Code = erl_prettypr:format(erl_syntax:form_list(Result)),
    file:write_file("../../../../out.txt", list_to_binary(Code)),
    file:write_file("../../../../out.ast", list_to_binary(io_lib:format("~p", [Result]))),
    true = string:str(Code, "#einfo{") > 0,
    true = string:str(Code, "/einfo.hrl") > 0.

module_with_include_lib(_) ->
    Path = filename:absname("../../../../test/pt_samples/module_with_include_lib.erl"),
    {ok, Ast} = epp:parse_file(Path, [], []),
    Result = einfo_pt:parse_transform(Ast, nil),
    Code = erl_prettypr:format(erl_syntax:form_list(Result)),
    file:write_file("../../../../out1.txt", list_to_binary(Code)),
    file:write_file("../../../../out1.ast", list_to_binary(io_lib:format("~p", [Result]))),
    true = string:str(Code, "#einfo{") > 0,
    true = string:str(Code, "/einfo.hrl") > 0.
