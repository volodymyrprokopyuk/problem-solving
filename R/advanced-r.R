# library(dplyr)
# library(rlang)
# library(lobstr)
# library(memoise)
# library(purrr)
# library(tibble)
# library(sloop)
# library(R6)
library(methods)

# df <- data.frame(runif(3), runif(3))
# names(df) <- c(1, 2)
# # df[[3]] <- df[[1]] + df[[2]]
# df$`3` <- df$`1` + df$`2`

# x <- runif(1e6)
# obj_size(x)
# y <- list(x, x, x)
# obj_size(y)

# a <- c(1, 5, 3, 2)
# b <- a
# b[[1]] <- 10

# x <- c(1, 2, 3)
# obj_addr(x)
# y <- x
# obj_addr(y)

# `_a` <- 1

# x <- c(1, 2, 3)
# y <- x
# obj_addr(x)
# obj_addr(y)
# y[[3]] <- 4
# obj_addr(y)

# f <- function(a) { a + 1 }
# x <- c(1, 2, 3)
# y <- f(x)
# obj_addr(x)
# obj_addr(y)

# l <- list(1, 2, 3)
# l2 <- l
# l2[[3]] <- 4
# ref(l)
# ref(l2)

# df <- data.frame(x = 1:3, y = 4:6)
# df[, 2] = df[, 2] * 2
# df[2, ] = df[2, ] * 2

# obj_size(letters)
# obj_size(ggplot2::diamonds)
# obj_size(list())
# obj_size(list(NULL, NULL, NULL))
# s <- "Vlad"
# v <- rep(s, 100)
# l <- list(rep(s, 100))
# obj_size(s)
# obj_size(v)
# obj_size(l)
# l <- list(mean, sd, var)
# obj_size(l)

# df <- data.frame(matrix(runif(20), ncol = 5))
# df
# median <- vapply(df, median, numeric(1))
# for (i in seq_along(median)) {
#   df[[i]] <- df[[i]] - median[[i]]
# }

# e1 <- rlang::env(a = 1, b = 2, c = 3)
# e2 <- e1
# e2$c
# e1$c <- 4
# e2$c

# e <- rlang::env()
# e$self <- e
# ref(e)

# x <- list()
# x
# x[[1]] <- x
# x
# ref(x)
# mem_used()

# a <- c(T, F, T)
# as.integer(a)
# as.numeric(a)
# as.double(a)
# sum(a)
# mean(a)

# display <- function(x) {
#   print(x)
#   invisible(x)
# }
# display(1)

# x <- 1:3
# attr(x, "a") <- "a value"
# attr(x, "b") <- "b value"
# attributes(x)
# y <- structure(1:4, a = "A value", b = "B value")

# x <- c(a = 1, b = 2, c = 3)
# y <- 1:3
# names(y) <- c("a", "b", "c")
# z <- setNames(1:3, c("a", "b", "c"))
# unname(z)
# names(z) <- NULL

# x <- 1:6
# dim(x) <- c(2, 3)
# dim(x) <- c(3, 2)

# x <- 1:3
# dim(x)
# str(x)
# y <- matrix(1:3, ncol = 1)
# str(y)
# z <- matrix(1:3, nrow = 1)
# str(z)
# u <- array(1:3, 3)
# str(u)

# 2 |> sqrt()
# quote(2 |> sqrt())
# substitute(2 |> sqrt())

# Sys.setenv("_R_USE_PIPEBIND_"=TRUE)
# a <- c("dogs", "cats", "rats")
# a |> {\(x) { grepl("at", x) }}()
# a |> x => grepl("at", x)

# a <- c("a", "b", "b", "a")
# f <- factor(a)
# o <- ordered(a, levels = c("b", "a"))
# class(f)
# typeof(f)
# attributes(f)

# a <- Sys.Date()
# typeof(a)
# class(a)
# attributes(a)
# a <- as.Date("1884-09-14")
# a <- as.POSIXct("1884-09-14: 12:34:56", tzone = "UTC")
# unclass(a)

# f <- factor(letters)
# levels(f) <- rev(levels(f))
# rev(factor(letters))
# factor(letters, levels = rev(letters))

# l <- list(list(1, 2), c(3, 4))
# m <- c(list(1, 2), c(3, 4))

# df <- data.frame(x = 1:3, y = c("a", "b", "c"))
# str(df)
# attributes(df)
# df2 <- tibble(x = 1:3, y = c("a", "b", "c"))
# str(df2)
# attributes(df2)
# tibble(x = 1:4, y = 1)
# data.frame(x = 1:4, y = 1:2)
# tibble(x = 1:3, y = x * 10)

# df3 <- data.frame(
#   age = c(35, 27, 18),
#   hair = c("blond", "brown", "black"),
#   row.names = c("Bob", "Susan", "Sam"))
# df3
# rownames(df3)
# df3["Bob", ]
# df4 <- tibble(df3, name = rownames_to_column(df3))
# tibble(df3)
# rownames_to_column(df3)
# as_tibble(df3, rownames = "nm")

# dplyr::starwars$height
# dplyr::starwars[, c("name", "height")]
# dplyr::starwars[[2]]
# dplyr::starwars[["height"]]
# dplyr::starwars[2]
# dplyr::starwars[c(2, 2, 2), ]
# dplyr::starwars[, 1:2]

# df <- data.frame(x = 1:3)
# df$y <- list(1:2, 1:3, 1:4)
# df <- data.frame(x = 1:3, y = I(list(1:2, 1:3, 1:4)))
# df
# t <- tibble(x = 1:3, y = list(1:2, 1:3, 1:4))

# v <- 1:4
# names(v) <- letters[v]
# v[c("a", "c")]

# l <- list(a = 1, b = 1:2, c = 1:3)
# l[c(1, 3)]
# l[c("a", "c")]
# l$a
# l <- matrix(1:6, nrow = 2)
# l[] <- 0

# x <- c(2.1, 4.2, 3.3, 5.4)
# str(x)
# typeof(x)
# class(x)
# attributes(x)
# x[c(3, 1)]
# sort(x)
# x[order(x)]
# x[c(1, 1)]
# x[x > 3]
# x[c(T, F, T, F, T)]
# x[]
# y <- setNames(x, letters[1:4])
# y[c("b", "d")]

# l <- matrix(1:9, nrow = 3)
# str(l)
# typeof(l)
# class(l)
# attributes(l)
# colnames(l) <- letters[1:3]
# l
# l[c(1, 3), ]
# l[c(T, F, T), c("a", "c")]
# l[1, 1, drop = F]
# l[c(1, 4, 7)]

# m <- outer(1:3, 1:3, FUN = "paste", sep = ",")
# i <- matrix(c(1, 1, 2, 2, 3, 3), ncol = 2, byrow = T)
# m[i]

# df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])
# df <- tibble(x = 1:3, y = 3:1, z = letters[1:3])
# df[df$x == 2, ]
# df[c("z")]
# df[, c("z"), drop = F]

# f <- factor(c("a", "b"))
# f[1, drop = T]

# m <- mtcars
# m
# m[m$cyl == 4, ]
# m[-(1:4), ]
# m[m$cyl <= 5, ]
# m[m$cyl == 4 | m$cyl == 6, ]

# x <- outer(1:5, 1:5, FUN = "*")
# lower.tri(4)
# x[lower.tri(x), drop = F]

# l <- matrix(1:16, nrow = 4)
# i <- upper.tri(l, diag = T) & lower.tri(l, diag = T)
# l[i]

# df <- tibble(x = 1:3, y = 4:6, z = c("a", "b", "c"))
# df[c(1, 2), c(1, 2)] <- NA
# df[is.na(df)] <- 0

# l <- list(1:3, "a", 4:6)
# l[[1]] <- 10:12
# l[[1]] <- NULL

# x <- c("m", "f", "u", "f", "f", "m", "m")
# lookup <- c(m = "Male", f = "Female", u = NA)
# unname(lookup[x])

# grades <- c(1, 2, 2, 3, 1)
# rules <- tibble(grade = 3:1, desc = c("Excellent", "Good", "Poor"), fail = c(F, F, T))
# i <- match(grades, rules$grade)
# rules[i, ]

# df <- tibble(x = c(1, 2, 3, 1, 2), y = 5:1, z = letters[1:5])
# df[sample(nrow(df)), ]
# df[sample(nrow(df), 3), ]
# df[order(df$x), ]

# x <- c("b", "c", "a")
# x[order(x)]
# sort(x)

# df <- tibble(x = c(2, 4, 1), y = c(9, 11, 6), n = c(3, 5, 1))
# df[rep(1:nrow(df), df$n), ]

# df <- data.frame(y = 1:3, z = 3:1, x = letters[1:3])
# df$z <- NULL
# str(df)
# df[setdiff(names(df), "z")]
# df[sample(ncol(df))]
# df[names(df) |> order()]

# unwhich <- function(x, n) {
#   r <- rep(F, n)
#   r[x] <- T
#   r
# }

# x <- sample(10) < 4
# y <- which(x)
# unwhich(y, 10)

# x1 <- 1:10 %% 2 == 0
# x2 <- which(x1)
# y1 <- 1:10 %% 5 == 0
# y2 <- which(y1)
# x1 & y1
# intersect(x2, y2)
# x1 | y1
# union(x2, y2)
# x1 & !y1
# setdiff(x2, y2)
# xor(x1, y1)
# setdiff(union(x2, y2), intersect(x2, y2))

# x <- c(1, 50, 20)
# y <- vector("list", length(x))
# for (i in seq_along(x)) {
#   y[[i]] <- rnorm(10, x[[i]])
# }

# x <- as.Date(c("2021-07-04", "2021-07-05"))
# for (i in seq_along(x)) { print(x[[i]]) }

# lapply(mtcars, function(x) unique(x))
# lapply(mtcars, \(x) unique(x))
# integrate(\(x) sin(x) ^ 2, 0, pi)

# l <- list(half = \(x) x / 2, double = \(x) x * 2)
# l$half(5)
# l$double(5)

# mean(1:10, na.rm = T)
# l <- list(1:10, na.rm = T)
# do.call(mean, l)
# (function() 1)()

# square <- \(x) x ^ 2
# deviation <- \(x) x - mean(x)
# x <- runif(100)
# sqrt(mean(square(deviation(x))))
# x |> deviation() |> square() |> mean() |> sqrt() -> y
# y <- x |> deviation() |> square() |> mean() |> sqrt()

# f <- \(x) x * 10
# (\() {
#   f <- 2
#   f(f)
# })()

# c <- 10
# c(c = c)

# f <- \(x) {
#   f <- \(x) {
#     f <- \() x^2
#     f() + 1
#   }
#   f(x) * 2
# }
# f(10)

# f <- function(x = 1, y = x + 1, z = a + b) {
#   a <- 10
#   b <- 20
#   c(x, y, z)
# }
# f()

# `%||%` <- \(a, b) if (is.null(a)) b else a
# x <- NULL
# x %||% "default"

# y <- 1
# f <- \(x = { y <- 2; 3 }, y = 0) c(x, y)
# f()

# f <- \(...) {
#   list(...)
#   # c(..3, ..2, ..1)
# }
# f(1, x = 2, 3, y = 4, 5, z = 6)

# f <- \(x) invisible(x)
# (f(1))

# f <- \(x) {
#   on.exit(cat("Bye\n"), add = T)
#   1
# }
# f()

# f <- \(d, c) {
#   od <- setwd(d)
#   on.exit(setwd(od), add = T)
#   c
# }
# getwd()
# f("~", getwd())
# getwd()

# f <- \() {
#   on.exit(message("a"), add = T, after = F)
#   on.exit(message("b"), add = T, after = F)
# }
# f()

# morley |> filter(Expt == 1) |> summary()
# summary(filter(morley, Expt == 1))

# `if`(F, "ok", "oh")
# `for`(i, 1:10, print(i))

# `(` <- \(x) if (is.numeric(x) && runif(1) < 0.1) x + 1 else x
# replicate(50, (1 + 2))

# l <- list(1, 2, 3)
# lapply(l, `+`, 10)

# `%+%` <- \(x, y) paste(x, y)
# "Vlad" %+% "Lana"

# `second<-` <- \(x, value) { x[2] <- value; x }
# x <- 1:5
# second(x) <- 20

# `modify<-` <- \(x, i, value) { x[i] <- value; x }
# x <- 1:5
# modify(x, 3) <- 30

# x <- c(a = 1, b = 2, c = 3)
# names(x)[2] <- "B"

# 1 + 2 + 3
# `+`(`+`(1, 2), 3)
# `+`(1, `+`(2, 3))

# x <- 1:5
# `if`(length(x) <= 5, x[[5]], x[[1]])

# `%+%` <- \(x, y) if (is.character(x) && is.character(y)) paste(x, y) else x + y
# 1 %+% 2
# "a" %+% "b"

# e1 <- env(
#   a = FALSE,
#   b = "a",
#   c = 2.3,
#   d = 1:3)
# env_print(e1)
# env_names(e1)

# x <- 1
# f <- \() x <<- 2
# f()

# where <- \(name, env = caller_env(), found = list()) {
#   if (identical(env, empty_env())) {
#     if (length(found) == 0) stop(name, " not found", call. = F)
#     found
#   }
#   else if (env_has(env, name)) {
#     found[[length(found) + 1]] <- env
#     where(name, env_parent(env), found)
#   }
#   else where(name, env_parent(env), found)
# }

# a <- 1
# where("a")
# mean <- 1
# where("mean")

# f <- \(x) g(x)
# g <- \(x) h(x)
# h <- \(x) { cst(); x }
# f(f(1))

# e <- new_environment()
# e$a <- 0
# set_a <- \(x) { o <- e$a; e$a <- x; invisible(o) }
# get_a <- \() e$a
# (set_a(1))
# get_a()

# f <- \(x) g(x)
# g <- \(x) h(x)
# h <- \(x) stop("oh")
# h <- \(x) abort("oh")
# f(1)

# try({ log(1); 1 })
# suppressMessages({ message("a"); message("b"); 1 })

# f <- \(x) { tryCatch(error = \(c) NA, log(x)) }
# f("a")

# f <- \() { tryCatch(message = \(c) message("caught"), { message("throw"); 1 })}
# f <- \() { tryCatch(
#   warning = \(c) warning("caught"),
#   { warning("throw"); 1 },
#   finally = { print("cleanup 1"); print("cleanup 2") })}
# f()

# f <- \() withCallingHandlers(
#   warning = \(c) print("-> warning"),
#   message = \(c) print("-> message"),
#   { warning("A warning"); message("A message") })
# f()

# f <- \() tryCatch(error = \(c) print("outer"), g())
# g <- \() tryCatch(error = \(c) print("innter"), stop("oh"))
# g <- \() stop("oh")
# f()

# f <- \() withCallingHandlers(error = \(c) print("outer"), g())
# g <- \() withCallingHandlers(error = \(c) print("innter"), stop("oh"))
# f()

show_condition <- \(code) tryCatch(
  error = \(c) "error",
  warning = \(c) "warning",
  message = \(c) "message",
  code
)
# show_condition(stop("oh"))
# show_condition(warning("caution"))
# show_condition(message("ok"))
# show_condition(1)

# tryCatch(
#   custom_condition = \(cnd) list(message = cnd$message, detail = cnd$detail),
#   abort(message = "Custom message", class = "custom_condition", detail = "Extra meta"))
#   abort(message = "Custom message", class = "custom_condition", detail = "Extra meta"))

# log_na <- \(x) tryCatch(error = \(cnd) NA, log(x))
# log_na(3)
# log_na("a")

# try2 <- \(code) tryCatch(error = \(cnd) structure(cnd$message, class = "try-error"), code)
# try2(1 + 2)
# try2(stop("oh"))
# try(stop("oh"))

# l <- c(1, 2)
# l <- append(l, "a", after = 1)
# l <- append(l, "b", after = 2)

# x <- runif(100)
# (b <- bench::mark(sqrt(x), x^0.5))

# map(1:3, \(x) x * 10)
# map(1:3, `*`, 10)
# lapply(1:3, `*`, 10)

# map_lgl(mtcars, is.double)
# map_int(mtcars, \(x) length(unique(x)))
# map_int(mtcars, \(x) x |> unique() |> length())
# map_int(mtcars, ~ length(unique(.x)))
# map_int(mtcars, ~ .x |> unique() |> length())
# map_dbl(mtcars, mean)

# x <- list(
#   list(-1, x = 1L, y = c(2, 3), z = "a"),
#   list(-2, x = 4L, y = c(5, 6), z = "b"),
#   list(-3, x = 8L, y = c(9, 10, 11)))
# map_int(x, "x")
# map_dbl(x, 1)
# map_dbl(x, list("y", 1))
# x |> map(list("y", 3), .default = NA)
# x |> map(list("x", 2))
# x |> map(c(3, 2))

# x <- list(a = 1:5, b = c(1:10, NA))
# map_dbl(x, ~ mean(.x, na.rm = T))
# map_dbl(x, mean, na.rm = T)

# map_dbl(rep(0, times = 4), `+`, runif(1))
# map_dbl(rep(0, times = 4), ~ .x + runif(1))

# trims <- c(0, 0.1, 0.2, 0.5)
# x <- rcauchy(10)
# map_dbl(trims, ~ mean(x, trim = .x))

# x <- list(
#   list(1, c(3, 9)),
#   list(c(3, 6), 7, c(4, 7, 6)))
# map(x, ~ map(.x, \(x) x * 10))

# mtcars |> split(mtcars$cyl) |>
#   map(~ lm(mpg ~ wt, data = .x)) |> map(coef) |> map_dbl("wt")

# x <- tibble(x = 1:3, y = 6:4)
# modify(x, ~ .x * 10)

# x <- map(1:4, ~ runif(10))
# w <- map(1:4, ~ rpois(10, 5))
# map2_dbl(x, w, weighted.mean)
# map2_dbl(1:4, 10, `*`)

# welcome <- \(x) cat("Welcome ", x, "!\n", sep = "")
# l <- c("Vlad", "Lana")
# (walk(l, welcome))

# r <- tempfile()
# s <- split(mtcars, mtcars$cyl)
# t <- file.path(r, paste0("cyl-", names(s), ".csv"))

# a <- c(a = 10, b = 20, c = 30)
# for (e in a) print(e)
# for (i in seq_along(a)) print(i)
# for (n in names(a)) print(n)
# imap(a, ~ sprintf("%i %s", ...))

# d <- tibble(iris)
# imap(d, ~ sprintf("%s %s", .x[[1]], .y))

# x <- map(1:6, ~ sample(1000, 4))
# imap_chr(x, ~ sprintf("%d %i", max(.x), .y))

# f <- \(x, y) y - x
# a <- list(y = c(10, 20, 30), x = c(1, 2, 3))
# a <- list(y = c(10, 20, 30), x = 1)
# pmap(a, f)

# tribble(
#   ~ n, ~ min, ~ max,
#   1L, 0, 1,
#   2L, 10, 100,
#   3L, 100, 1000)

# set_names(c("foo", "bar")) |> map_chr(paste0, ":suffix")
# mtcars |> map_dbl(sum)
# iris |> map_if(is.factor, as.character, .else = as.integer)
# a <- tibble(a = c(1, 2, 3), b = c(10, 20, 30), c = c(100, 200, 300))
# a |> modify(~ .x + 1)
# add1 <- \(x) x + 1
# a |> modify(~ .x |> add1() |> add1())
# a |> modify(add1) |> modify(add1)
# a |> modify_if(~ all(.x > 50), ~ .x + 1, .else = ~ .x - 1)
# a |> modify_at("b", add1)
# a |> modify_in(c(2, 2), add1)
# a |> modify_in(list("b", 2), add1)
# a |> pmap(~ ..1 + ..2 + ..3)
# a |> pmap(\(a, b, c) a + b + c)
# a |> pluck("b", 2) <- -99
# a |> modify(2)

# l <- map(1:4, ~ sample(1:5, 5, replace = T))
# str(l)
# reduce(l, intersect)
# reduce(l, union)
# accumulate(l, intersect)
# reduce(1:5, `+`)
# accumulate(1:5, `+`)

# sum(integer())
# prod(integer())
# min(integer())
# max(integer())

# a <- tibble(
#   num1 = c(0, 10, 20),
#   num2 = c(5, 6, 7),
#   chr1 = c("a", "b", "c"))
# a |> map_if(is.numeric, mean)

# make_power <- \(e) { force(e); \(x) x^e }
# square <- make_power(2)
# cube <- make_power(3)
# square(2)
# cube(2)
# x <- 2
# sq <- make_power(x)
# x <- 3
# sq(2)

# make_counter <- \(init = 0) { c <- init; \() (c <<- c + 1) }
# c1 <- make_counter()
# c2 <- make_counter(10)
# c1()
# c1()
# c2()
# c2()
# c1()
# c2()

# chatty <- \(f) {
#   force(f)
#   \(...) { cat(..., "\n"); f(...) }
# }
# f <- chatty(\(x, y) x + y)
# f(1, 2)

# f <- \(x) message(x)
# sf <- quietly(f)
# sf(1)

# x <- list(
#   c(0.512, 0.165, 0.717),
#   c(0.064, 0.781, 0.427),
#   c(0.890, 0.785, 0.495),
#   "oh")
# (r <- rep(NA, length(x)))
# try(for (i in seq_along(x)) r[[i]] <- sum(x[[i]]))
# x |> map(safely(sum)) |> transpose() -> y
# y$error |> map_lgl(is.null) -> yr
# y$result[yr]

# f <- \(x) { Sys.sleep(1); x }
# mf <- memoise(f)
# system.time(mf(1))
# system.time(mf(1))

# fib <- \(n) if (n < 2) 1 else fib(n - 2) + fib(n - 1)
# fib <- memoise(fib)
# system.time(fib(30))

dot_every <- \(f, n) {
  force(f); force(n)
  i <- 0
  \(...) {
    if (i %% n == 0) cat(".")
    f(...)
    i <<- i + 1
  }
}

delay_by <- \(f, n) {
  force(f); force(n)
  \(...) { Sys.sleep(n); f(...) }
}

# 1:80 |> map_dbl(identity |> delay_by(0.1) |> dot_every(10))

new_date <- \(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

# new_date(c(-1, 0, 1))

new_difftime <- \(x, units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))
  structure(x, class = "difftime", units = units)
}

# new_difftime(c(1, 10, 3600), "secs")
# new_difftime(53, "weeks")

new_factor <- \(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))
  structure(x, class = "factor", levels = levels)
}

validate_factor <- \(x) {
  v <- unclass(x)
  l <- attr(x, "levels")
  if (any(is.na(v) | any(v < 0)))
    stop("all values must be non-missing and non-negative")
  if (length(l) < max(v))
    stop("some values have messing levels")
  x
}

a_factor <- \(x, ...) x |> new_factor(...) |> validate_factor()

# f <- a_factor(c(2, 1, 1, 3) |> as.integer(), levels = c("a", "b", "c"))

new_num <- \(x) {
  stopifnot(is.double(x))
  structure(x, class = "num")
}

display <- \(x, e) UseMethod("display", x)

display.num <- \(x, e) print(paste("Displaying", x, e))

# 1 |> new_num() |> display("...")

new_secret <- \(x = double(), ..., class = character()) {
  stopifnot(is.double(x))
  structure(x, class = c(class, "secret"))
}

print.secret <- \(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

`[.secret` <- \(x, i) NextMethod() |> new_secret()

new_supersecret <- \(x) new_secret(x, class = "supersecret")

print.supersecret <- \(x, ...) {
  print(strrep("xxx", nchar(x)))
  invisible(x)
}

# c(15, 1, 456) |> new_supersecret()

# Accumulator <- R6Class("Accumulator", list(
#   acc = 0,
#   inc = \(x = 1) {
#     self$acc <- self$acc + x
#     invisible(self)
#   }
# ))

# a <- Accumulator$new()
# a$inc()$inc(2)$acc

# Person <- R6Class("Person", list(
#   name = NULL,
#   age = NA,
#   initialize = \(name, age = 0) {
#     stopifnot(is.character(name), length(name) == 1)
#     stopifnot(is.numeric(age), length(age) == 1)
#     self$name = name
#     self$age = age
#   },
#   print = \() {
#     sprintf("<Person> %s %d", self$name, self$age) |> cat()
#     invisible(self)
#   }
# ))

# p <- Person$new("Vlad", 36)

# Accumulator2 <- R6Class(
#   "Accumulator2",
#   inherit = Accumulator,
#   public = list(
#     inc = \(x = 1) {
#       cat("Adding ", x, "\n", sep = "")
#       super$inc(x = x)
#     }
#   )
# )

# a2 <- Accumulator2$new()
# a2$inc()$inc(2)$acc
# class(a2)
# names(a2)
# attributes(a2)

# Random <- R6Class("Random", active = list(
#   random = \(v) if (missing(v)) runif(1) else stop("Read-only field")
# ))

# r <- Random$new()
# r$random
# r$random

# Person <- R6Class(
#   "Person",
#   private = list(.name = NULL, .age = NA),
#   active = list(
#     age = \(v) if (missing(v)) private$.age else stop("age is read-only"),
#     name = \(v)
#     if (missing(v))
#       private$.name
#     else {
#       stopifnot(is.character(v), length(v) == 1)
#       private$.name <- v
#       self
#     }
#   ),
#   public = list(
#     initialize = \(name, age = 0) {
#       private$.name = name
#       private$.age = age
#     }
#   )
# )

# p <- Person$new("Vlad", 36)
# p2 <- p$clone()
# p$name <- "Volodymyr"

# Resource <- R6Class(
#   "Resource",
#   public = list(
#     initialize = \() print("Initializing..."),
#     finalize = \() print("Cleaning up...")
#   )
# )

# r <- Resource$new()

setClass(
  "Person",
  slots = c(
    name = "character",
    age = "numeric"
  )
)

Person <- \(name, age = 0) {
  age <- as.double(age)
  new("Person", name = name, age = age)
}

p <- new("Person", name = "Vlad", age = 36)
# p@name
# slot(p, "age")

setGeneric("age", \(x) standardGeneric("age"))
setGeneric("age<-", \(x, value) standardGeneric("age<-"))
setMethod("age", "Person", \(x) x@age)
setMethod("age<-", "Person", \(x, value) { x@age <- value; x })

# age(p) <- 37
# age(p)

setClass(
  "Employee",
  contains = "Person",
  slots = c(manager = "Person"),
  prototype = list(manager = new("Person"))
)

e <- new("Employee")
# str(e)

setValidity(
  "Person",
  \(object) {
    if (length(object@name) != length(object@age))
      "@name and @age must be of the same length"
    else T
  }
)

p2 <- Person("Lana", 28)
# str(p2)

setMethod(
  "show", "Person",
  \(object) sprintf("<%s> %s %d", is(object)[[1]], object@name, object@age) |> print()
)

p2
