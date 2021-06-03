# Zsh

- Interactive login shell with line editing (zle), code completion (compsys),
  history mechanism, spelling correction, prompt theme or a shell script processor with
  dataflow programming and first-class files, commands, processes and pipes
- Shell `builtin` vs shell `function` vs external `command`
- Builtins
  - `set` options
  - `alias al=cmd` prefer `function` definition over `alias` text substitution
  - `IFS="_ " read a b c d _ <<< "A_B_C D E_F"; echo "^$a, $b, $c, $d$"` read stdin into
    shell variables
  - `echo`
  - `printf '%c' {a..z} $'\n'`
  - `exec fd> file` redirects stderr for all subsequent commands
  - `exec fd< file` opens file for reading from the fd
  - `exec fd> file` opnes file for writing from the fd
  - `exec fd<&-`, `exec fd>&-` closes input / output fd
- Command -> pipeline -> sublist -> list
  - Simple command
    - `command` `-s` short option `--long` long option `-` arguments
    - `exec cmd` replaces the current shell with the `cmd` by running the `cmd` in the
      current process instread of forking a sub-process. `trap ... EXIT` is not invoked
  - Pipeline (one-way communication)
    - `command` stdout 1 `|` 0 stdin `command2`
    - `command` stdout 1 + stderr 2 `|&` 0 stdin `command2` equivalent to `2>&1 |`
  - Sublist
    - `pipeline && pipeline2` on success runs `pipeline2`
    - `pipeline || pipeline2` on failure runs `pipeline2`
  - List
    - Set of sublists terminated by `\n`, `;` sequencing, `&`, `&|`, `&!` last pipeline
      in the background
- Complex command
  - `if list then list [elif list then list] ... [else list] fi` executes `then` on zero
    exit status
    - `if list { list } [elif list { list }] ... [else { list }]`
  - `for name ... in word ...; do list done` iterates over words
    - `for name ... (word ...) list`
  - `for (( init; stop; next )) do list done` stops iteration when `stop` is zero
    - `for (( init; stop; next )) list`
  - `while / until list do list done` iterates while the `list` is zero / non-zero
    - `while / until list { list }`
  - `case word in pat [| pat] ... list ;; ... esac`
    - `case word { pat [| pat] ... list ;; ...}`
  - `{ list }` executes in the current shell
  - `( list )` executes in a subshell with `trap reset`
  - `{ try-list } always { always-list }` returns the `try-list` exit status after
    execuing the `always-list`
  - `function word { list } [2>&1]` define function with stream redirection
  - `[[ expr ]]` returns zero exit status if the `expr` is true
- Redirection (order matters as file discriptors are pointers, redirection can be
  anywhere in a simple command or can precede or follow a complex command)
  - `< file` open file for reading from stdin
  - `> file` open a file for writing + truncate from stdout
  - `>> file` open a file for writing + append from stdout
  - `&> file`, `&>> file` redirects both stdout and stderr to file + truncate / append
    (`&>` = `> file 2>&1`)
  - `<&fd` duplicates stdin from fd
  - `>&fd` duplicates stdout to fd
  - `fd<&-`, `fd>&-` closes input / output fd
  - `{param}>&1`, `{param}>&-` open / close fd using parameter
    - E. g. `exec {stdout}>&1; echo ok >&$stdout; exec {stdout}>&-`
  - `<&$param`, `>&$param` read / write fd using parameter
    - E. g. `exec {stdin}<&0; read v <&$stdin; echo $v; exec {stdin}<&-`
  - File / stdin literal with parameter substitution and command substitution
    - `<<< "string"` here-string as stdin
    - `<< EOD\n ... \nEOD` here-document as stdin

- Brace expansion `{1,2,3}`, `{1..10}`, `{a,b}{1,2}`
- Parameter substitution `${1}`
- Command substitution `$(list)` returns command output rather than status code
- Process substitution `<(list)`, `=(list)`, `>(list)` creates anonymous named pipe
