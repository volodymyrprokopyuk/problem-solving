library(nycflights13)
library(tidyverse)

# p <- mpg |> ggplot() +
  # geom_point(aes(x = displ, y = hwy), color = "green") +
  # geom_point(aes(x = displ, y = cty), color = "orange")
  # geom_point(aes(x = displ, y = hwy, color = class)) +
  # geom_point(aes(x = displ, y = hwy, shape = class)) +
  # geom_point(aes(x = displ, y = hwy, color = displ > 5 & hwy > 21))
  # geom_point(aes(x = displ, y = hwy)) + facet_wrap(~ class, ncol = 2)
  # geom_point(aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
  # geom_point(aes(x = displ, y = hwy)) + facet_grid(drv ~ .)
  # geom_point(aes(x = displ, y = hwy, color = drv)) +
  # geom_smooth(aes(x = displ, y = hwy, linetype = drv, color = drv))
  # geom_smooth(aes(x = displ, y = hwy, group = drv))
  # geom_point(aes(x = displ, y = hwy), position = "jitter")
  # geom_boxplot(aes(x = class, y = hwy)) + coord_flip()

# p <- mpg |> ggplot(aes(x = displ, y = hwy)) +
#   geom_point(aes(color = class)) + geom_smooth()

# p <- diamonds |> ggplot() +
  # geom_bar(aes(x = cut))
  # stat_count(aes(x = cut))
  # geom_bar(aes(x = cut, y = stat(prop), group = 1))
  # stat_summary(aes(x = cut, y = depth), fun.min = min, fun.max = max, fun = median)
  # geom_bar(aes(x = cut, fill = cut))
  # geom_bar(aes(x = cut, fill = clarity), position = "dodge")
  # geom_bar(aes(x = cut, fill = cut), show.legend = F, width = 1) +
  # theme(aspect.ratio = 1) + labs(x = NULL, y = NULL) +
  # coord_flip() + coord_polar()

# d <- tribble(
#   ~a, ~b,
#   "a", 20,
#   "b", 30,
#   "c", 40
# )

# p <- d |> ggplot() + geom_bar(aes(x = a, y = b), stat = "identity")

# svg("plot.svg")
# print(p)
# dev.off()

# flights |> dplyr::filter(month == 1 & day == 5)
# flights |> dplyr::filter(month %in% c(11, 12))
# flights |> dplyr::filter(!(arr_delay > 120 | dep_delay > 120))
# flights |> dplyr::filter(arr_delay >= 2)
# flights |> dplyr::filter(dest %in% c("IAH", "HOU"))
# flights |> dplyr::filter(month == 2 & between(day, 1, 2))
# flights |> dplyr::filter(is.na(dep_time))
