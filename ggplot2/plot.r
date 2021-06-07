library(ggplot2)

p <- ggplot(data.frame(x = c(-15, 15)), aes(x = x)) +
    stat_function(fun = function(x) { x^2 })

# mpg
# p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
# p <- ggplot(mpg, aes(cty, hwy)) + geom_point()
# p <- ggplot(mpg, aes(model, manufacturer)) + geom_point()
# p <- ggplot(mpg, aes(cty)) + geom_histogram()
# p <- ggplot(mpg, aes(displ, cty, color = class)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy, shape = drv)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy, size = cyl)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color = "blue"))
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point(color = "blue")

# diamonds
# p <- ggplot(diamonds, aes(carat, price)) + geom_point()

# economics
# p <- ggplot(economics, aes(date, unemploy)) + geom_line()


pdf("plot.pdf")
print(p)
dev.off()
