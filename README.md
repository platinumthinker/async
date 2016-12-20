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
Skip unknown file: "/home/thinker/develop/async/src/aa/ee/dd/4913"
Load module one

2> one:
module_info/0  module_info/1  test/0         
2> one:test().
six

Load module async_compiler
Eval event: {inotify,close_write,false,"4913","./src/"}
Eval event: {inotify,close_write,false,"async_compiler.erl","./src/"}
ha ha! 
Skip unknown file: "/home/thinker/develop/async/src/4913"
ha ha! 
```

## features:
- [x] Recompile changes erl file
- [ ] Support other files (hrl, dtl, lfe, elixir)
- [ ] User callback for filetype and unknown (other) filetypes
- [ ] Notification for recompile files (and users callback)
- [ ] Run eunit, dialyzer, elvis, user callback after reload file (optional)
- [ ] Integrate with rebar (use options from rebar.config)
- [ ] Patches other nodes in cluster
- [ ] Writes changes on disk (beam files)
