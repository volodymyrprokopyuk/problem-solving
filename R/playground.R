# library(ggplot2)

# for (i in 5:8) { cat("->", i, "\n") }

# a <- 2.3
# x <- (6 * a + 42) / 3 ^ (4.2 - 3.62)

# x <- sqrt(0.5 * (25.2 + 15 + 16.44 + 15.3 + 18.6) / 5)

# x <- 3 ^ 2 + 4 ^ (1 / 8)
# x <- x / 2.33
# x <- -8.2e-13

# v <- c(1, 2.3, 4e5)
# v2 <- c(6, 7.8, 9e0)
# v3 <- c(v, v2)

# s <- 3:27
# s <- 3:-27
# s <- seq(1, -20, -2)
# s <- seq(from = 1, to = -10, length.out = 6)
# s <- rep(1, 4)
# s <- rep(c(1, 2, 3), times = 5)
# s <- rep(c(1, 2, 3), each = 2)
# s <- sort(c(5, 3, 8, 2, 4, 1, 7, 9, 6, 0), decreasing = TRUE)
# l <- length(s)
# s <- seq(5, -11, by = -0.3)
# s <- seq(-11, 5, by = 0.3)
# s <- rep(c(-1, 3, 5), times = 2, each = 3)
# s <- sort(s, decreasing = TRUE)

# v <- c(5, -2.3, 4, 4, 4, 6, 8, 10, 40221, -8)
# x <- v[1]
# x <- v[length(v)]
# x <- v[-(length(v))]
# x <- v[c(1, 3, 5)]
# x <- v[1:4]
# x <- v[4:1]
# v[c(1, 3, 5)] <- c(10, 20, 30)

# v <- c(seq(3, 6, length.out = 5), rep(c(2, -5.1, -33), times = 2), 7/42 + 2)
# a <- v[c(1, length(v))]
# b <- v[-c(1, length(v))]
# d <- c(a[1], b, a[length(a)])
# e <- sort(a, decreasing = TRUE)
# f <- e[length(e):1]
# g <- c(rep(v[3], times = 3), rep(v[6], times = 4), v[length(v)])
# g2 <- v[c(rep(3, times = 3), rep(6, times = 4), length(v))]
# v2 <- v
# v2[c(1, 5:7, length(v2))] <- 99:95

# s <- 1:3
# s
# t <- seq(10, 90, by = 10)
# t
# u <- t - s
# u
# v <- u + 100
# v
# sum(s)
# prod(s)
# t[2:5] <- c(1, 2, 3)
# t

# v <- c(2,0.5,1,2,0.5,1,2,0.5,1)
# v[1:length(v)] <- 1

# F <- c(45, 77, 20, 19, 101, 120, 212)
# C <- 5/9 * (F - 32)

# v1 = c(2,4,6)
# v2 = c(1,2)
# v3 = c(v1, v1 * 2)
# v3 = rep(v1, times = 2) * rep(v2, each = 3)
# v3[2:5] <- c(-0.1, -100)

# m <- matrix(c(-3, 2, 893, 0.17), nrow = 2, ncol = 2)
# m2 <- matrix(c(-3, 2, 893, 0.17), nrow = 2, ncol = 2, byrow = TRUE)
# m <- cbind(1:3, 4:6)
# m <- rbind(c(1, 3, 4), 5:3, c(100, 20, 90), 11:13)
# m <- matrix(c(0.3, 4.5, 55.3, 91, 0.1, 105.5, -4.2, 8.2, 27.9), nrow=3, ncol=3)
# m[3, 2]
# m[5]
# m[2,]
# m[,2]
# m[2:3, 1:2]
# m[, c(3, 1)]
# diag(m)
# m[-2, 2:3]
# m2 <- m
# m2[2,] <- 1:3
# m2[c(1, 3), 2] <- 900
# m2[, 3] <- m2[3,]
# m2[c(1, 3), c(1, 3)] = c(7, -7)
# m2[c(3, 1), 2:1] <- c(99, -99)
# diag(m2) <- 0

# m <- matrix(c(4.3, 3.1, 8.2, 8.2, 3.2, 0.9, 1.6, 6.5), nrow = 4, byrow = TRUE)
# dim(m[-1,])
# n <- m
# n[, 2] <- sort(n[, 2])
# matrix(n[-4, -1])
# l <- n[3:4,]
# n[c(4, 1), c(2, 1)] <- diag(l) * -1/2

# m <- rbind(c(2, 5, 2), c(6, 1, 4))
# t(m)
# diag(4)
# diag(m)
# m * 2
# m <- matrix(c(3, 4, 1, 2), nrow = 2)
# n <- solve(m)
# m %*% n

# a <- cbind(c(1, 2, 7), c(2, 4, 6))
# b <- cbind(c(10, 30, 40), c(20, 40, 60))
# 2/7 * (a - b)

# a <- matrix(c(1, 2, 7))
# b <- matrix(c(3, 4, 8))
# t(a) %*% b
# t(b) * (a %*% t(a))
# b %*% (t(a) %*% a)

# a <- array(1:24, dim = c(3, 4, 2))
# a
# a[2, , 2]
# a[2, c(3, 1), 2]
# a[1, ,]

# a <- array(seq(4.8, 0.1, length = 48), dim = c(4, 2, 6))
# b <- a[c(4, 1), 2,]
# c <- array(rep(b[2,], times = 4), dim = c(2, 2, 2, 3))
# d <- a[, , -6]
# d[c(2, 4), 2, c(1, 3, 5)] <- -99

# a <- c(3, 2, 1, 4, 1, 2, 1, -1, 0, 3)
# b <- c(4, 1, 2, 1, 1, 0, 0, 3, 0, 4)
# c <- a == b
# all(c)

# a <- c(6, 9, 7, 3, 6, 7, 9, 6, 3, 6, 6, 7, 1, 9, 1)
# a == 6
# 6 != a
# b <- a[-(1:3)]
# c <- array(b, dim = c(2, 2, 3))
# c * 2 <= 6/2 + 4
# diag(10) == 0
# any(diag(10) == 0)

# a <- c(T, T, F)
# b <- c(F, T, T)
# a && b
# ! a

# a <- c(7, 1, 7, 10, 5, 9, 10, 3, 10, 8)
# a > 5 | a == 2
# b <- c(8, 8, 4, 4, 5, 1, 5, 6, 6, 8)
# a <= 6 | a != 4
# c <- a + b
# c >= 14 & a != 15

# a <- c(5, -2.3, 4, 4, 4, 6, 8, 10, 40221, -8)
# a[c(2, 10)]
# a[a < 0]
# a[c(T, F)]
# a[-which(a < 0)]
# a <- matrix(c(0.3, 4.5, 55.3, 91, 0.1, 105.5, -4.2, 8.2, 27.9), nrow=3)
# which(a > 25)
# which(a > 25, arr.ind = T)
# a[a < 1] <- 99
# b <- c(a[, 1], a[, 2], a[, 3])
# which(b > 25)

# a <- c(7, 5, 6, 1, 2, 10, 8, 3, 8, 2)
# b <- a[a >= 5]
# a[-which(a >= 5)]
# d <- matrix(b, nrow = 2, byrow = T)
# d[d == 8] <- d[1, 2] ^ 2
# all(d <= 25 & d > 4)

# a <- array(c(10, 5, 1, 4, 7, 4, 3, 3, 1, 3, 4, 3, 1, 7, 8, 3, 7, 3), dim = c(3, 2, 3))
# which(a == 3 | a == 4, arr.ind = T)
# a[a < 3 | a >= 7] <- 100
# a[c(F, T)]

# cat("one", "two", "three", 45, "\n", sep = "_")
# paste("one", "two", "three", 45, "\n", sep = "_")
# sprintf("%d %s", 1, "one")
# strsplit("Vlad and Lana", "\\s+")
# grepl("(\\w+) (\\d+)", c("one 1", "two 2", "11 eleven", "ten 10"))
# gregexpr("(\\w+) (\\d+)", c("one 1", "two 2", "11 eleven", "ten 10"))
# gregexec("(\\w+) (\\d+)", c("one 1", "two 2", "11 eleven", "ten 10"))
# gsub("(\\w+) (\\d+)", "\\2 \\1", c("one 1", "two 2", "11 eleven", "ten 10"))

# sprintf("The result of %.2f x %.2f = %.2f", 4, 0.75, 3)
# s <- sub("V\\w+", "Lana", "Vlad and Lana")
# gsub("[A-Z]\\w{3}", "Vlad", s)

# fname <- c("Liz", "Jolene", "Susan", "Boris", "Rochelle", "Tim", "Simon", "Amy")
# snum <- c(0, 0, 0, 1, 0, 1, 1, 0)
# schar <-  c("female", "female", "female", "male", "female", "male", "male", "female")
# snumfac <- factor(snum)
# levels(snumfac)
# scharfac <- factor(schar)
# levels(scharfac)
# levels(snumfac) <- c("1", "2")
# snumfac == "2"
# fname[snumfac == "2"]
# fname[scharfac == "male"]

# mob <- c("Apr", "Jan", "Dec", "Sep", "Nov", "Jul", "Jul", "Jun")
# ms <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# mob.fac <- factor(mob, levels = ms, ordered = T)
# mob.fac[2]
# mob.fac[4] < mob.fac[5]

# mob2.fac <- factor(c("Oct", "Feb", "Feb"), levels = ms, ordered = T)
# c(mob.fac, mob2.fac)

# a <- c(0.53, 5.4, 1.5, 3.33, 0.45, 0.01, 2, 4.2, 1.99, 1.01)
# l <- c("small", "medium", "large")
# f <- cut(a, breaks = c(0, 2, 4, 6), right = F, include.lowest = T, labels = l)

# s <- rep("M", times = 20)
# s[c(1, 5:7, 12, 14:16)] <- "F"
# sf <- factor(s)
# p <- rep("national", times = 20)
# p[c(1, 4, 12, 15, 16, 19)] <- "labour"
# p[c(6, 9, 11)] <- "green"
# p[c(10, 20)] <- "other"
# pf <- factor(p)
# pf[sf == "M"]
# sf[pf == "national"]
# s2 <- c("M", "M", "F", "F", "F", "M")
# sf <- c(sf, factor(s2))
# p2 <- c("National", "Maori", "Maori", "Labour", "Greens", "Labour")
# pf <- c(pf, factor(p2))
# con <- c(93, 55, 29, 100, 52, 84, 56, 0, 33, 52, 35, 53, 55, 46, 40, 40, 56, 45, 64,
#   31, 10, 29, 40, 95, 18, 61)
# l = c("low", "moderate", "high")
# conf <- cut(con, breaks = c(0, 30, 70, 100), include.lowest = T, labels = l)
# conf
# conf[pf == "labour"]
# conf[pf == "national"]

# l <- list(matrix(data=1:4, nrow=2), c(T, F, T, T), "hello")
# l[[1]][, 2]
# l[1:2]
# names(l) <- c("mat", "log", "str")
# l$mat[, 2]
# names(l)
# l[[4]] <- list(1:4)
# l$fifth <- list("ok")
# l[[4]][[1]][2]

# l <- list(seq(-4, 4, length = 20), matrix(c(F, T, T, T, F, T, T, F, F), nrow = 3),
#   c("don", "quixote"), factor(c("LOW", "MED", "LOW", "MED", "MED", "HIGH")))
# l[[2]][2:1, 2:3]
# l[[3]][1] <- sub("don", "Don", l[[3]][1])
# l[[3]][2] <- sub("quixote", "Quixote", l[[3]][2])
# l[[1]][l[[1]] > 1]
# which(l[[4]] == "MED")

# l2 <- list(fac = factor(c("LOW", "MED", "LOW", "MED", "MED", "HIGH")),
#   num = c(3, 2.1, 3.3, 4, 1.5, 4.9), old = l[1:3])
# l2$fac[l2$num >= 3]
# l2$flag <- rep(l2$old[[2]][, 3], times = 2)
# l2$num[l2$flag]
# l2$old[[3]] <- "Don Quixote"

# df <-  data.frame(
#   person = c("Peter", "Lois", "Meg", "Chris", "Stewie"),
#   age = c(42, 40, 17, 14, 1),
#   sex = factor(c("M", "F", "F", "M", "M")))
# df[1:2, 2:3]
# df[, c(1, 3)]
# df$person[3]
# nrow(df)
# ncol(df)
# dim(df)
# df$person
# df2 <- data.frame(person = "Brian", age = 7, sex = factor("M", levels = levels(df$sex)))
# df <- rbind(df, df2)
# ff <- factor(c("High","High","Low","Med","High","Med"))
# df <- cbind(df, funny = ff)
# df$age.mon <- df$age * 12
# df[df$sex == "M", -3]
# df[df$age > 10 | df$funny == "High",]
# df[df$age > 45,]

# df <- data.frame(
#   person = c("Stan", "Francine", "Steve", "Roger", "Hayley", "Klaus"),
#   sex = factor(c("M", "F", "M", "M", "F", "M")),
#   funny = factor(c("High", "Med", "Low", "High", "Med", "Med")))
# df$age <- c(41, 41, 15, 21, 60, 1600)
# df <- cbind(df, age = c(41, 41, 15, 21, 60, 1600))
# df <- df[, c(1, 4, 2, 3)]
# df <- data.frame(person = df$person, age= df$age, sex = df$sex, funny = df$funny)
# df[df$sex == "F" & (df$funny == "High" | df$funny == "Med"), c("person", "age")]
# df[grep("^S.+", df$person),]

# a <- c(13563, -14156, -14319, 16981, 12921, 11979, 9568, 8833, -12968, 8133)
# a[is.infinite(a ^ 75)]
# a[-which(is.infinite(a ^ 75) & a < 0)]

# a <- matrix(c(77875.4, -35466.25, -39803.81, 27551.45, -73333.85, 55976.34, 23764.3,
#   36599.69, 76694.82, -36478.88, -70585.69, 47032), nrow=3)
# which(is.nan(a ^ 65 / Inf), arr.ind = T)
# all(a[!is.nan(a ^ 67 + Inf)] == a[a ^ 67 != -Inf])
# a[a ^ 67 == -Inf | is.finite(a ^ 67)]

# a <-  c(NA, 5.89, Inf, NA, 9.43, -2.35, NaN, 2.10, -8.53, -7.58, NA, -4.58, 2.01, NaN)
# a[-which(is.na(a))]
# na.omit(a)

# a <- c(4.3, 2.2, NULL, 2.4, NaN, 3.3, 3.1, NULL, 3.4, NA)
# length(a)
# is.na(a)
# which(is.na(a))
# is.null(a)

# l <- list(c(7, 7, NA, 3, NA, 1, 1, 5, NA))
# names(l) <- c("alpha")
# is.null(l$beta)
# l$beta <- which(is.na(l$alpha))

# a <- matrix(1:9, nrow = 3)
# attributes(a)$dim
# attr(a, "dim")
# class(a)

# class(1:4)
# class(seq(1, 4, length = 6))
# as.logical(c("0", "1", "1"))

# a <- matrix(2:13, nrow = 3)
# as.character(as.vector(t(a)))
# a <- cbind(c(34, 23, 33, 42, 41), c(0, 1, 1, 0, 0), c(1, 2, 1, 1, 2))
# a <- as.data.frame(a)
# a$V2 <- as.logical(a$V2)
# a[, 3] <- as.factor(a[, 3])

# x <- c(1.1, 2, 3.5, 3.9, 4.2)
# y <- c(2, 2.2, -1.3, 0, 0.2)
# a <- cbind(x, y)
# p <- plot(x, y)
# p <- plot(a, type = "b", main = "Main plot", xlab = "X axis", ylab = "Y axis",
#   col = "seagreen4")
# print(p)

# x <- c(1.1, 2, 3.5, 3.9, 4.2)
# y <- c(2, 2.2, -1.3, 0, 0.2)
# p <- qplot(x, y) + ggtitle("ggplot2 example plot") + xlab("x axis") + ylab("y axis")
# df <- data.frame(x = x, y = y)
# p <- ggplot(df, aes(x, y)) +
#   geom_point(size = 3, shape = 6, color = "blue") +
#   geom_line(linetype = 2, color = "red")

# x <- 1:20
# y <- c(-1.49, 3.37, 2.59, -2.78, -3.94, -0.92, 6.43, 8.51, 3.41, -8.23, -12.01, -6.58,
#   2.87, 14.12, 9.63, -4.58, -14.78, -11.67, 1.17, 15.62)
# a <- rep("standard", length(x))
# a[y >= 5] <- "big"
# a[y <= -5] <- "small"
# a[(x >= 5 & x <= 15) & (y > - 5 & y < 5)] <- "sweet"
# af <- factor(a)
# df <- data.frame(x = x, y = y, af = af)
# p <- ggplot(df, aes(x, y, shape = af, color = af)) + geom_point(size = 3) +
#   geom_line(aes(group = 1), color = "gray", linetype = 2, size = 0.8) +
#   geom_hline(yintercept = c(-5, 5), color = "red") +
#   geom_segment(x = 5, y = -5, xend = 5, yend = 5, color = "red", linetype = 4,
#     size = 0.5) +
#   geom_segment(x = 15, y = -5, xend = 15, yend = 5, color = "red", linetype = 4,
#     size = 0.5)

# pdf("plot.pdf")
# svg("plot.svg")
# png("plot.png")
# print(p)
# dev.off()

# ggsave("plot.png", plot = p, width = 40, height = 30, units = "cm")

# p <- read.table("person.dat", header = T, sep = " ", na.strings = "*")
# write.table(p, "person2.dat", sep = "_", na = "?", quote = F, row.name = F)
