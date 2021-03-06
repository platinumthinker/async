# async
Sync inspired developer helper tools but very customizing.

## Requares:
Async uses intotifywait, fsevent_watch or fanotify_watch (it depends on the type of OS) for monitoring FS and rebar for compile this project.

## For build:
```
rebar get-deps compile
```

## For try it:
```
rebar shell

1> application:ensure_all_started(async).
2> async:watch(".").
Error in "tc_member.erl" => compile_with_error

erl => module tc_member recompile
```

## API:
* async:watch(Dir)   - watching src in user directory
* async:unwatch(Dir) - unwatch for user directory
* async:pause()   - pause and collecting changes
* async:unpause() - unpause and compile all changes
* async:forget_changes() - forget chenges files (useful in pause)

## Options:
```
{async, [
  %% Default state for pause after start async
  {pause, true},
  {plugins, [ my_plugin1, my_plugin2 ]},
  %% inotify extra events (see man inotifywait)
  {events, []},
  %% addition path for monitoring changes
  {paths, [ "/home/develop/my_lib" ]},
  %% include in code path for successful compile
  {include_path, "/usr/lib/erlang/*"},
  {exclude_path_regexp, [ "/usr/lib/erlang/*" ]},
  %% interval in ms between processed changes files
  {collect_interval, 2000},

  %%% User callbacks run after async system callbacks
  %% Format {Action :: atom(), [{Module, Function}]} or
  %%        {Action :: atom(),  {Module, Function}} or
  %%        {Action :: atom(),  {cmd, Command}}
  %% when Module:Function is fun/1,
  %%      Command is string for run programm
  %% Mandatory run
  {init,        {erlang, display}},

  %% Optional
  {change,      [{erlang, display}] },
  {compile,     {erlang, display}},
  {pre_load,    {erlang, display}},
  {load,        [{erlang, display}, {eunit, test}] },
  {after_load,  [{erlang, display}, {cmd, "notify-send"}] },
  {error,       [{cmd, "notify-send"}]},

  %% Mandatory run
  {'end',         {erlang, display}}
]}.
```

## features:
- [x] Support rebar3 projects
- [x] Recompile changes erl,hrl,dtl file
- [x] User callback for filetype and unknown (other) filetypes
- [x] User command from OS for filetype and unknown (other) filetypes
- [x] Run eunit, dialyzer, elvis, user callback after reload file (optional)
- [ ] Support other files (lfe, elixir)
- [ ] Notification for recompile files (and users callback)
- [ ] Patches other nodes in cluster
- [ ] Writes changes on disk (beam files)
- [ ] Support custom options for compiler
