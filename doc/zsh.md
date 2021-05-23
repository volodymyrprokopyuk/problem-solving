# Zsh

- Interactive login shell with line editing (zle), code completion (compsys),
  history mechanism, spelling correction, prompt theme or a shell script processor
- Command
  - Invocation `command` `-s` short option `--long` long option `-` arguments
- Pipe (only one direction)
  - `stdout | stdin`
  - `stdout + stderr |& stdin` equivalent to `2>&1 |`
- Command substitution `$(cmd)` (only one direction)
- Process substitution `<(cmd)`, `>(cmd)` (only one direction)

- Co-process `coproc` starts a job in the background and communicates with it from the
  parent shell via `print -p` and `read -p` without using named pipes
- `expect` and `zpty` use pseudo-terminals to avoid buffering deadlocks when interacting
  with commands
