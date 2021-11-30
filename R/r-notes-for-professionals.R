# m <- matrix(data = 1:6, nrow = 2)
# m
# n <- matrix(data = 1:6, nrow = 2, byrow = T)
# n

# m <- matrix(data = c(T, F), nrow = 2, ncol = 3)
# m
# n <- matrix(data = c(T, F), nrow = 2, ncol = 3, byrow = T)
# n

# m <- matrix(letters[1:6], nrow = 2)
# rownames(m) <- c("R1", "R2")
# colnames(m) <- c("C1", "C2", "C3")
# m
# class(m)
# str(m)
# is.matrix(m)
# is.vector(m)
# as.vector(m) |> length()

# lm(formula = mpg ~ wt, data = mtcars) |> coef()
# lm(formula = mpg ~ 0 + wt, data = mtcars) |> coef()
# lm(formula = mpg ~ wt + 0, data = mtcars) |> coef()
# lm(formula = mpg ~ wt -1, data = mtcars) |> coef()

print("Vlad")
cat("Vlad\n")
