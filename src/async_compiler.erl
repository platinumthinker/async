%%%----------------------------------------------------------------------------
%%% @author platinumthinker <platinumthinker@gmail.com>
%%% @doc
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(async_compiler).

-export([
    processing_file/1
]).

-spec processing_file(Path :: file:filename_all()) ->
    {Modile :: module(), Bin :: binary(),  Errors :: string()} | nothing.

processing_file(Path) ->
    async_lib:chain([
                     fun classificate_file/1,
                     fun read_config/1,
                     fun compile/1,
                     fun load_binary/1
                    ], Path).

classificate_file(File) ->
    classificate_file(File, filename:extension(File)).

classificate_file(File, ".erl") -> {ok, {erl, File, fun compile_erl/1}};
classificate_file(File, _Other) ->
    io:format("Skip unknown file: ~p~n", [File]),
    {done, ok}.

read_config({_FileType, File, CompileFun}) ->
    Opts = [],
    {ok, {{File, Opts}, CompileFun}}.

compile({Args, CompileFun}) -> CompileFun(Args).

-spec compile_erl({Path :: file:filename_all(), Opts :: []}) ->
    {ok, {Module :: module(), Bin :: binary()}} |
    {error, _Reason}.
compile_erl({Path, Opts}) ->
    NOpts = lists:usort([bin_opt_info, return, binary, verbose | Opts]),
    case compile:file(Path, NOpts) of
        {ok, Module, Binary, _Warnings} -> {ok, {Module, Path, Binary}};
        {error, Errors, _Warnings}      -> {error, Errors}
    end.

load_binary({Module, Path, Binary}) ->
    case code:load_binary(Module, Path, Binary) of
        {module, Module} ->
            io:format("Load module ~p~n", [Module]),
            {ok, Module};
        Err -> Err
    end.

% [
%  {"./src/async_server.erl",
%   [
%    {88, erl_lint, {unused_var,'File'}},
%    {92, erl_lint, {format_error,
%                    {"wrong number of arguments in format call", []}}}
%   ]
%  }
% ]
