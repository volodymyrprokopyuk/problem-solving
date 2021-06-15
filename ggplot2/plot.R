library(ggplot2)
# library(dplyr)
# library(maps)
# library(mgcv)
# library(nlme)

# p <- ggplot(data.frame(x = c(-15, 15)), aes(x = x)) +
#     stat_function(fun = function(x) { x^2 })

# mpg
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
# p <- ggplot(mpg, aes(displ, hwy, color = factor(cyl))) + geom_point()

# diamonds
# p <- ggplot(diamonds, aes(carat, price)) + geom_point()
# mod_coef = coef(lm(log10(price) ~ log10(carat), data = diamonds))
# p <- ggplot(diamonds, aes(log10(carat), log10(price))) + geom_bin2d() +
#   geom_abline(intercept = mod_coef[1], slope = mod_coef[2], color = "white", size = 1) +
#   facet_wrap(~cut, nrow = 1)
# p <- ggplot(mpg, aes(class, fill = drv)) + geom_bar()
# p <- ggplot(mpg, aes(class)) + geom_bar(aes(fill = drv))
# p <- ggplot(mpg, aes(class, fill = hwy, group = hwy)) + geom_bar()
# p <- ggplot(diamonds, aes(depth)) + geom_histogram(binwidth = 0.1) + xlim(55, 70)
# p <- ggplot(diamonds, aes(depth)) +
#   geom_freqpoly(aes(color = cut), binwidth = 0.1) + xlim(58, 68)
# p <- ggplot(diamonds, aes(depth)) +
#   geom_histogram(aes(color = cut), binwidth = 0.1, position = "fill") + xlim(58, 68)
# p <- ggplot(diamonds, aes(depth)) + geom_density() + xlim(58, 68)
# p <- ggplot(diamonds, aes(depth, color = cut, fill = cut)) +
#   geom_density(alpha = 0.1) + xlim(58, 68)
# p <- ggplot(diamonds, aes(clarity, depth)) + geom_boxplot()
# p <- ggplot(diamonds, aes(clarity, depth)) +
#   geom_boxplot(aes(group = cut_width(carat, 0.3)))
# p <- ggplot(diamonds, aes(clarity, depth)) + geom_violin()
# p <- ggplot(diamonds, aes(clarity, depth)) +
#   geom_violin(aes(group = cut_width(carat, 0.3)))

# economics
# p <- ggplot(economics, aes(date, unemploy)) + geom_line()
# p <- ggplot(economics, aes(date, unemploy / pop)) + geom_line()
# p <- ggplot(economics, aes(date, uempmed)) + geom_line()
# p <- ggplot(economics, aes(unemploy / pop, uempmed)) + geom_point() + geom_path()
# year <- function(x) as.POSIXlt(x)$year + 1900
# p <- ggplot(economics, aes(unemploy / pop, uempmed)) +
#   geom_point(aes(color = year(date))) + geom_path(color = "gray50")
# pres <- subset(presidential, start > economics$date[1])
# pres
# p <- ggplot(economics) +
#   geom_rect(aes(xmin = start, xmax = end, fill = party),
#     data = pres, ymin = -Inf, ymax = Inf, alpha = 0.2) +
#   geom_vline(aes(xintercept = as.numeric(start)), data = pres, color = "gray50",
#     alpha = 0.7) +
#   geom_text(aes(x = start, y = 700, label = name),
#     data = pres, size = 3, hjust = "left", nudge_x = 100) +
#   geom_line(aes(date, unemploy)) +
#   scale_fill_manual(values = c("blue", "red"))
# caption <- paste(strwrap("Unemployment rates in the US have varied a lot over the years",
#   40), collapse = "\n")
# df <- data.frame(x = min(economics$date), y = max(economics$unemploy), caption = caption)
# p <- ggplot(economics, aes(date, unemploy)) + geom_line() +
#   geom_text(aes(x, y, label = caption), data = df, size = 5, hjust = "inward",
#     vjust = "inward")
# p <- ggplot(economics, aes(date, unemploy)) + geom_line() +
#   annotate("text", min(economics$date), max(economics$unemploy), label = caption,
#     size = 5, hjust = "inward", vjust = "inward")

# df <- data.frame(x = c(3, 1, 5), y = c(2, 4, 6), label = c("a", "b", "c"))
# p <- ggplot(df, aes(x, y, label = label)) + labs(x = NULL, y = NULL) +
#   ggtitle("Basic plot types") +
#   # geom_point() geom_text() geom_bar(stat = "identity") geom_tile()
#   # geom_line() geom_area() geom_path() geom_polygon()

# df <- data.frame(x = 1, y = 3:1, family = c("sans", "serif", "mono"))
# p <- ggplot(df, aes(x, y)) + geom_text(aes(label = family, family = family,
#   fontface = "italic"))

# df <- data.frame(x = c(1, 1, 2, 2, 1.5), y = c(1, 2, 1, 2, 1.5),
#   text = c("bottom-lett", "top-left", "bottom-right", "top-right", "center"))
# p <- ggplot(df, aes(x, y)) + geom_text(aes(label = text), hjust = "inward",
#   vjust = "inward", size = 10, angle = 30)

# df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5))
# p <- ggplot(df, aes(resp, trt)) + geom_point() +
#   geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.1) + xlim(1, 3.6)

# faithfuld
# df <- data.frame(waiting = c(55, 80), eruptions = c(2, 4.3),
#   label = c("peak one", "peak two"))
# p <- ggplot(faithfuld, aes(waiting, eruptions)) + geom_tile(aes(fill = density)) +
#   geom_label(data = df, aes(label = label))
# p <- ggplot(faithfuld, aes(eruptions, waiting)) +
#   geom_contour(aes(z = density, color = ..level..))
# p <- ggplot(faithfuld, aes(eruptions, waiting)) +
#   geom_raster(aes(fill = density))
# ff <- faithfuld[seq(1, nrow(faithfuld), by = 10), ]
# p <- ggplot(ff, aes(eruptions, waiting)) +
#   geom_point(aes(size = density), alpha = 1/3) + scale_size_area()

# p <- ggplot(mpg, aes(displ, hwy, color = class)) + geom_point(show.legend = FALSE) +
#   directlabels::geom_dl(aes(label = class), method = "smart.grid")

# head(Oxboys)
# p <- ggplot(Oxboys, aes(age, height, group = Subject)) + geom_point() + geom_line()
# p <- ggplot(Oxboys, aes(age, height)) + geom_line(aes(group = Subject)) +
#   geom_smooth(method = "lm", se = FALSE, size = 2)
# p <- ggplot(Oxboys, aes(Occasion, height)) + geom_boxplot() +
#   geom_line(aes(group = Subject), color = "blue", alpha = 0.5)

# df <- data.frame(x = 1:3, y = 1:3, color = c(1, 3, 5))
# p <- ggplot(df, aes(x, y, color = factor(color))) +
#   geom_line(aes(group = 1), size = 2) + geom_point(size = 5)
# p <- ggplot(df, aes(x, y, color = color)) +
#   geom_line(aes(group = 1), size = 2) + geom_point(size = 5)

# df <- data.frame(x = rnorm(2000), y = rnorm(2000))
# p <- ggplot(df, aes(x, y)) + geom_point(shape = ".")
# p <- ggplot(df, aes(x, y)) + geom_point(alpha = 1/5)
# p <- ggplot(df, aes(x, y)) + geom_bin2d(bins = 20)
# p <- ggplot(df, aes(x, y)) + geom_hex(bins = 30)

pdf("plot.pdf")
print(p)
dev.off()
