# Zsh

- Interactive login shell with line editing (zle), code completion (compsys),
  history mechanism, spelling correction, prompt theme or a shell script processor with
  dataflow programming and first-class files, commands, processes and pipes
- Shell `builtin` vs shell `function` vs external `command`
- Builtins
  - `set` options
  - `alias al=cmd` prefer `function` over `alias` text substitution
  - `IFS="_ " read a b c d _ <<< "A_B_C D E_F"; echo "^$a, $b, $c, $d$"` read stdin into
    shell variables
  - `echo`
  - `printf '%c' {a..z} $'\n'`
  - `exec 2> file` redirects stderr for all subsequent commands
  - `exec 3< file` opens file for reading from the file descriptor 3
  - `exec 4> file` opnes file for writing from the file descriptor 4
  - `exec 3>&-` closes the file discriptor 3
  - `exec cmd` replaces the shell with the `cmd`
- Command -> pipeline -> sublist -> list
  - Simple command
    - `command` `-s` short option `--long` long option `-` arguments
    - `exec command` runs the `command` in the current process instread of forking a
      sub-process by replacing a the current shell. `trap ... EXIT` is not invoked
  - Pipeline (one-way communication)
    - `command` stdout 1 `|` 0 stdin `command2`
    - `command` stdout 1 + stderr 2 `|&` 0 stdin `command2` equivalent to `2>&1 |`
  - Sublist (only one direction)
    - `pipeline && pipeline2` on success
    - `pipeline || pipeline2` on failure
  - List
    - Set of sublists terminated by `\n`, `;` sequencing, `&`, `&|`, `&!` last pipeline
      in the background
- Complex command
  - `if list then list [elif list then list] ... [else list] fi` execute `then` on zero
    exit status
    - `if list { list } [elif list { list }] ... [else { list }]`
  - `for name ... in word ...; do list done` iterate over words
    - `for name ... (word ...) list`
  - `for (( init; stop; next )) do list done` stop iteration when `stop` is zero
    - `for (( init; stop; next )) list`
  - `while / until list do list done` iterate while the `list` is zero / non-zero
    - `while / until list { list }`
  - `case word in pat [| pat] ... list ;; ... esac`
    - `case word { pat [| pat] ... list ;; ...}`
  - `( list )` execute in a subshell with `trap reset`
  - `{ list }` execute in the current shell
  - `{ try-list } always { always-list }` returns the `try-list` exit status after
    execuing the `always-list`
  - `function word { list } [2>&1]` define function with stream redirection
  - `[[ expr ]]` returns zero exit status if the `expr` is true
- Redirection (order matters as file discriptors are pointers, redirection can be
  anywhere in the command)
  - `< file` open file for reading from stdin
  - `> file` open a file for writing + truncate from stdout
  - `>> file` open a file for writing + append from stdout
  - `&> file`, `&>> file` redirects both stdout and stderr to file + truncate / append
    (`&>` = `> file 2>&1`)
  - `<& number` duplicate stdin from file descriptor
  - `>& number` duplicate stdout to file descriptor
  - `number >&-` close file descriptor
  - File / stdin literal with parameter and command substitution
    - `<<< "string"` here-string as stdin
    - `<< EOD\n ... \nEOD` here-document as stdin

- Brace expansion `{1,2,3}`, `{1..10}`, `{a,b}{1,2}`
- Parameter substitution `${1}`
- Command substitution `$(cmd)` returns command output rather than status code
- Process substitution `<(cmd)`, `=(cmd)`, `>(cmd)` creates anonymous named pipe

- Co-process `coproc` starts a job in the background and communicates with it from the
  parent shell via `print -p` and `read -p` without using named pipes
- `expect` and `zpty` use pseudo-terminals to avoid buffering deadlocks when interacting
  with commands
