# async
Sync inspired developer helper tools but very customizing.

## Requares:
Sync uses intotify-tools for monitoring FS and rebar for compile this project.

## For build:
```
rebar get-deps compile
```

## For try it:
```
rebar shell

1> application:ensure_all_started(async).
2>
Error in "tc_member.erl" =>
 compile_with_error

erl => module tc_member recompile
```

## Options:
```
{async, [
  {plugins, [ my_plugin1, my_plugin2 ]},
  %% inotify events (see man inotifywait)
  {events, []},
  %% addition path for monitoring changes
  {paths, [ "/home/develop/my_lib" ]},
  {inotify_opts, [{exclude, "\\.tags"}, {exclude, "my_file"}]},
  %% interval between processed changes files
  {collect_interval, 2000}
]}.
```

## features:
- [x] Recompile changes erl,hrl file
- [ ] Support other files (dtl, lfe, elixir)
- [x] User callback for filetype and unknown (other) filetypes
- [ ] Notification for recompile files (and users callback)
- [ ] Run eunit, dialyzer, elvis, user callback after reload file (optional)
- [ ] Integrate with rebar (use options from rebar.config)
- [ ] Patches other nodes in cluster
- [ ] Writes changes on disk (beam files)
