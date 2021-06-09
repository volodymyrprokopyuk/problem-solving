library("ggplot2")
library("mgcv")

# p <- ggplot(data.frame(x = c(-15, 15)), aes(x = x)) +
#     stat_function(fun = function(x) { x^2 })

mpg
# p <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point()
# p <- ggplot(mpg, aes(cty, hwy)) + geom_point()
# p <- ggplot(mpg, aes(model, manufacturer)) + geom_point()
# p <- ggplot(mpg, aes(cty)) + geom_histogram()
# p <- ggplot(mpg, aes(displ, cty, color = class)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy, shape = drv)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy, size = cyl)) + geom_point()
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point(color = "blue")
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~class)
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~cyl)
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
#     geom_smooth(method = "loess", span = 0.2)
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
#     geom_smooth(method = "gam", formula = y ~ s(x))
# p <- ggplot(mpg, aes(displ, hwy)) + geom_point() +
#     geom_smooth(method = "lm")
# p <- ggplot(mpg, aes(drv, hwy)) + geom_jitter()
# p <- ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
# p <- ggplot(mpg, aes(drv, hwy)) + geom_violin()
# p <- ggplot(mpg, aes(hwy)) + geom_histogram()
# p <- ggplot(mpg, aes(hwy)) + geom_freqpoly()
# p <- ggplot(mpg, aes(hwy)) + geom_freqpoly(binwidth = 2.5)
# p <- ggplot(mpg, aes(hwy)) + geom_density()
# p <- ggplot(mpg, aes(displ, color = drv)) + geom_freqpoly(binwidth = 0.5)
# p <- ggplot(mpg, aes(displ, fill = drv)) + geom_histogram(binwidth = 0.5) +
#   facet_wrap(~drv, ncol = 1)
# p <- ggplot(mpg, aes(manufacturer)) + geom_bar()
# drug <- data.frame(drug = c("a", "b", "c"), effect = c(4.2, 9.7, 6.1))
# drug
# p <- ggplot(drug, aes(drug, effect)) + geom_bar(stat = "identity")
# p <- ggplot(drug, aes(drug, effect)) + geom_point()
# p <- ggplot(mpg, aes(cty, hwy)) + geom_point(alpha = 1 / 5) +
#   xlab("City driving (mpg)") + ylab("Highway driving (mpg)")
# p <- ggplot(mpg, aes(drv, hwy)) + geom_jitter(width = 0.25) +
#   xlim("4", "f", "r") + ylim(0, 50)
p <- ggplot(mpg, aes(displ, hwy, color = factor(cyl))) + geom_point()

# diamonds
# p <- ggplot(diamonds, aes(carat, price)) + geom_point()

# economics
# p <- ggplot(economics, aes(date, unemploy)) + geom_line()
# p <- ggplot(economics, aes(date, unemploy / pop)) + geom_line()
# p <- ggplot(economics, aes(date, uempmed)) + geom_line()
# p <- ggplot(economics, aes(unemploy / pop, uempmed)) + geom_point() + geom_path()
# year <- function(x) as.POSIXlt(x)$year + 1900
# p <- ggplot(economics, aes(unemploy / pop, uempmed)) +
#   geom_point(aes(color = year(date))) + geom_path(color = "gray50")

pdf("plot.pdf")
print(p)
dev.off()
