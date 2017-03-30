-module (async_hrl).

-include_lib("systools/include/inotify.hrl").

-export([
         filetypes/0,
         init/1,
         terminate/1,
         change/2,
         compile/2,
         pre_load/2,
         after_load/2
        ]).

-record(s, {
          src_files = [] :: list(string())
         }).

-spec filetypes() -> [async_plugin:filetype()].
filetypes() -> [ "hrl" ].

-spec init([async_plugin:opt()]) -> #s{}.
init(_Opts) ->
    ExcludePaths = [".", filename:join(code:root_dir(), "lib")],
    SrcFiles = lists:foldl(
        fun(Dir, Acc) ->
            Res = wildcard(filename:join(Dir, "src"), ".*\\.(erl|dtl|lfe|ex)$"),
            Res ++ Acc
        end, [], [filename:dirname(X) || X <- code:get_path() -- ExcludePaths]),
    #s{src_files = SrcFiles}.

-spec terminate(#s{}) -> ok.
terminate(_State) -> ok.

-spec change({async_plugin:filetype(),
             async_plugin:filename(),
             async_plugin:event()}, #s{}) ->
    {ok, [{async_plugin:filetype(),
           async_plugin:filename(),
           [async_plugin:opt()]}]}.
change({"hrl", FileChange, Event}, #s{src_files = SrcFiles}) ->
    io:format("hrl => module ~ts recompile~n",
              [filename:basename(FileChange, ".hrl")]),
    Changes = who_include(FileChange, SrcFiles),
    lists:foreach(fun(F) -> async_server:event({F, Event}) end, Changes),
    {done, []}.

-spec compile({_, _, _}, _) -> {done, ok}.
compile({_, _, _}, _) -> {done, ok}.

-spec pre_load(_, #s{}) -> ok.
pre_load({_, _, _}, _) -> ok.

-spec after_load(_, #s{}) -> ok.
after_load(_, _) -> ok.

who_include(HrlFile, SrcFiles) ->
    HrlFileBaseName = filename:basename(HrlFile),
    Pred = fun(SrcFile) ->
        {ok, Forms} = epp_dodger:parse_file(SrcFile),
        is_include(HrlFileBaseName, Forms)
    end,
    lists:filter(Pred, SrcFiles).

is_include(_HrlFile, []) -> false;
is_include(HrlFile, [{tree, attribute, _,
                      {attribute, _, [{_, _, IncludeFile}]}} | Forms])
  when is_list(IncludeFile) ->
    IncludeFileBaseName = filename:basename(IncludeFile),
    case IncludeFileBaseName of
        HrlFile -> true;
        _ -> is_include(HrlFile, Forms)
    end;
is_include(HrlFile, [_SomeForm | Forms]) ->
    is_include(HrlFile, Forms).

wildcard(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(Y, Acc1) -> [Y | Acc1] end, []).

