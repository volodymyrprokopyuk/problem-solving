library(lobstr)
library(tibble)

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

l <- matrix(1:6, nrow = 2)
l[] <- 0
l
