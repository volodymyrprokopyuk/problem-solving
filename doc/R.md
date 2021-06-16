# R

- `?func`, show help `??"search"` search help
- Literals `NULL`, `NA`, `Inf`, `-Inf`
- Copy `a <- b`
- Vector (element) `c`, `length`, `seq`, `rep`, `sort`, `which`
    - Subsetting (1-based) `v[1]` element, `v[c(1, 2)]`, `v[c(T, F)]`, `v[v > 0]`
      sub-vector, `v[-c(...)]`, v[-which(v < 0)] remove
- Matrix (column-first, row x column) `matrix`, `cbind`, `rbind`, `dim`, `nrow`,
  `ncol`, `t`, `%*%`, `solve`
    - Subsetting `m[1]` column-first vector, `m[1, 1]` element, `m[1,]` row, `m[, 1]`
      column, `m[c(...), c(...)]` sub-matrix, `m[-c(...), -c(...)]` remove, `diag`
      diagonal
- Array (row x column x layter x block) `array`
- Logical `TRUE`, `FALSE`, `T`, `F`, `any`, `all`, `T && || F`, `v & | v`, `! v`
- String `nchar`, `cat`, `paste`, `sprintf`, `strsplit`, `substr`, `grep[l]`,
  `[g]regexpr`, `regexec`, `[g]sub`
- Factor `factor`, `levels`

- Functions `ls`, `getwd`, `setwd`
- Evaluation `eval(parse(text = "1 + 2"))`
- Errors: `message`, `warning`, `stop`
