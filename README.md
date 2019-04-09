# Build, Run, Watch

Building, running, & watching utilities for projects with a server & client.

You can use this library to help you write scripts that build projects, run
executables, and watch source files for changes(triggering re-builds).


## TODO

* Watcher module for watching files and running/re-running commands.
* Purescript/Spago Module.
* Ghcid & Pscid modules w/ datatypes for arguments to pass. Ghcid should write
  json to file, we watch the file, parse the json & log it. Pscid should be
  temporary, eventually just launch a psc ide server and communicate w/ it
  using json & file watching.
* BuildRunWatch top-level module with high-level functionality. You should be
  able to pass some spec to the main function in this module and it will handle
  argument parsing, building, watching, and help text printing.
* Brick UI with single window or split client/server panes, shortcuts for
  forcing rebuilds or restarting watchers, build result indicators,
  scrollable-logs, etc.


## Motivation

I have a few projects that are Haskell servers & Javascript/Elm/Purescript
clients. This usually means building & serving takes up 2-3 terminals. One for
building/running the Haskell project, one for building/watching the Client
code, and one for watching/serving/hot-reloading the Client code.

Instead of using three terminals and splitting my attention between them all, I
would usually write a Haskell script that launches all the processes & restarts
them when their configuration changes(e.g., `stack.yaml`, `webpack.config.js`,
& `package.json`).

After essentially re-writing the same script multiple times, I decided to build
this library to keep me from repeating myself & allow upgrades in future
build/management scripts to easily be backported to my prior projects.


## License

BSD-3-Clause
