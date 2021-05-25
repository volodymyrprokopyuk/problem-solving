# Zsh

- Interactive login shell with line editing (zle), code completion (compsys),
  history mechanism, spelling correction, prompt theme or a shell script processor with
  dataflow programming and first-class files, commands, processes and pipes
- Command -> pipeline -> sublist -> list
  - Simple command
    - `command` `-s` short option `--long` long option `-` arguments
    - `exec command` runs the `command` in the current process instread of forking a
      sub-process by replacing a the current shell. `trap ... EXIT` is not invoked
  - Pipeline (only one direction)
    - `command` stdout 1 `|` 0 stdin `command2`
    - `command` stdout 1 + stderr 2 `|&` 0 stdin `command2` equivalent to `2>&1 |`
  - Sublist (only one direction)
    - `pipeline && pipeline2` on success, `pipeline || pipeline2` on failure
  - List
    - Set of sublists terminated by `\n`, `;` sequencing, `&`, `&|`, `&!` last pipeline
      in the background
- Complex command
  - `if list then list [elif list] ... [else list] fi` execute `then` on zero exit
    status
  - `for name ... in word ...; do list done` iterate over words
  - `for (( init; stop; next )) do list done` stop iteration when `stop` is zero
  - `while / until list do list done` iterate while the `list` is zero / non-zero
  - `case word in pat [| pat] ... list ;; esac`
  - `( list )` execute in a subshell with `trap reset`
  - `{ list }` execute in the current shell
  - `{ try-list } always { always-list }` returns the `try-list` exit status after
    execuing the `always-list`
  - `function word { list } [2>&1]` define function with stream redirection
  - `[[ expr ]]` returns zero exit status if the `expr` is true

- Shell `builtin` vs shell `function` vs external `command`
- Command substitution `$(cmd)` (only one direction)
- Process substitution `<(cmd)`, `>(cmd)` (only one direction)

- Co-process `coproc` starts a job in the background and communicates with it from the
  parent shell via `print -p` and `read -p` without using named pipes
- `expect` and `zpty` use pseudo-terminals to avoid buffering deadlocks when interacting
  with commands
