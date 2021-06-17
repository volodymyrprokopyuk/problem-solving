# R

- `?func`, show help `??"search"` search help
- Literals `NULL`, `NA`, `Inf`, `-Inf`
- Copy `a <- b`
- Vector (homogeneous, element) `c`, `length`, `seq`, `rep`, `sort`, `which`
    - Subsetting (1-based) `v[1]` element, `v[c(1, 2)]`, `v[c(T, F)]`, `v[v > 0]`
      sub-vector, `v[-c(...)]`, v[-which(v < 0)] remove
- Matrix (homogeneous, column-first, row x column) `matrix`, `cbind`, `rbind`, `dim`,
  `nrow`, `ncol`, `t`, `%*%`, `solve`
    - Subsetting `m[1]` column-first vector, `m[1, 1]` element, `m[1,]` row, `m[, 1]`
      column, `m[c(...), c(...)]` sub-matrix, `m[-c(...), -c(...)]` remove, `diag`
      diagonal
- Array (homogeneous, row x column x layter x block) `array` + array subsetting
- Logical `TRUE`, `FALSE`, `T`, `F`, `any`, `all`, `T && || F`, `v & | v`, `! v`
- String `nchar`, `cat`, `paste`, `sprintf`, `strsplit`, `substr`, `grep[l]`,
  `[g]regexpr`, `regexec`, `[g]sub`
- Factor (string vector with ordering) `factor`, `levels`, `c`, `cut`, `length` + factor
  subsetting
- List (heterogeneous, nested) `list`, `length` `l[[1]]` member reference -> object,
  `l[1]` list slicing -> list, `names`, `l$name`
- Data frame (hegerogeneous named list of equal vectors, observation records = rows of
  variables = columns) `data.frame`, `nrow`, `ncol`, `dim`, `rbind`
    - Subsetting `df[1, 1]`, `df[c(1), c("name")]`, `df$name`, `df[df$name > 0,]`

- Functions `ls`, `getwd`, `setwd`
- Evaluation `eval(parse(text = "1 + 2"))`
- Errors: `message`, `warning`, `stop`
