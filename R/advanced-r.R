# library(tibble)
# library(dplyr)
# library(rlang)
# library(lobstr)

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

# show_condition <- \(code) tryCatch(
#   error = \(c) "error",
#   warning = \(c) "warning",
#   message = \(c) "message",
#   code
# )
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

x <- runif(100)
(b <- bench::mark(sqrt(x), x^0.5))
