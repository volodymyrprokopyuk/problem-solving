# library(nycflights13)
suppressMessages(library(tidyverse))
# library(svglite)
# library(lubridate, warn.conflicts = F)
# library(jsonlite, warn.conflicts = F)
# library(forcats)
# library(bench)
library(stringr)

# batting <- as_tibble(Lahman::Batting)

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

# mpg |> ggplot(aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
#   geom_boxplot() + coord_flip() -> p

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

# flights |> dplyr::filter(month == 1 & day == 5)
# flights |> dplyr::filter(month %in% c(11, 12))
# flights |> dplyr::filter(!(arr_delay > 120 | dep_delay > 120))
# flights |> dplyr::filter(arr_delay >= 2)
# flights |> dplyr::filter(dest %in% c("IAH", "HOU"))
# flights |> dplyr::filter(month == 2 & between(day, 1, 2))
# flights |> dplyr::filter(is.na(dep_time))
# flights |> arrange(desc(month), desc(day))
# flights |> arrange(desc(arr_delay))
# flights |> select(-c(year, month, day, flight))
# flights |> rename(yr = year)
# flights |> select(time_hour, air_time, everything())
# flights |> select(year:day, ends_with("delay"), distance, air_time) |>
#   mutate(gain = arr_delay - dep_delay, speed = distance / air_time * 60) |>
#   select(gain, speed, everything())
# flights |> summarize(delay = mean(dep_delay, na.rm = T))
# flights |> group_by(year, month, day) |> summarize(delay = mean(dep_delay, na.rm = T))
# flights |> group_by(dest) |>
#   summarize(
#     count = n(),
#     dist = mean(distance, na.rm = T),
#     delay = mean(arr_delay, na.rm = T)) |>
#   dplyr::filter(count > 20, dest != "HNL") |>
#   ggplot(aes(x = dist, y = delay)) +
#   geom_point(aes(size = count), alpha = 1/3) +
#   geom_smooth(se = F) -> p
# flights |> dplyr::filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
#   group_by(tailnum) |> summarize(delay = mean(arr_delay, na.rm = T)) |>
#   ggplot(aes(x = delay)) + geom_freqpoly(binwidth = 1) -> p
# flights |> dplyr::filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
#   group_by(tailnum) |>
#   summarize(
#     delay = mean(arr_delay, na.rm = T),
#     count = n()) |>
#   dplyr::filter(count > 25) |>
#   ggplot(aes(x = count, y = delay)) + geom_point(alpha = 1/10) -> p
# flights |> dplyr::filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
#   group_by(year, month, day) |>
#   summarize(delay = mean(arr_delay), delay_pos = mean(arr_delay[arr_delay > 0]))
# flights |> dplyr::filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
#   group_by(dest) |>
#   summarize(dist_sd = distance |> sd()) |>
#   arrange(desc(dist_sd))
# flights |> dplyr::filter(!is.na(dep_delay) & !is.na(arr_delay)) |>
#   group_by(dest) |>
#   summarize(carriers = n_distinct(carrier)) |>
#   arrange(desc(carriers))
# flights |> mutate(
#   cancelled = is.na(dep_time)) |>
#   ggplot(aes(x = sched_dep_time)) +
#   geom_freqpoly(aes(color = cancelled), binwidth = 1/4) -> p

# batting |> group_by(playerID) |>
#   summarize(
#     ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
#     ab = sum(AB, na.rm = T)) |>
#   dplyr::filter(ab > 100) |>
#   ggplot(aes(x = ab, y = ba)) + geom_point() + geom_smooth(se = F) -> p

# diamonds |> ggplot() + geom_bar(aes(x = cut)) -> p
# diamonds |> ggplot() + geom_histogram(aes(x = carat), binwidth = 0.5) -> p
# diamonds |> dplyr::filter(carat < 3) |>
#   ggplot(aes(x = carat)) + geom_histogram(binwidth = 0.1) -> p
# diamonds |> dplyr::filter(carat < 3) |>
#   ggplot(aes(x = carat, color = cut)) + geom_freqpoly(binwidth = 0.1) -> p
# diamonds |> dplyr::filter(carat < 3) |>
#   ggplot(aes(x = carat)) + geom_histogram(binwidth = 0.01) -> p
# diamonds |> ggplot() +
#   layer(
#     mapping = aes(x = carat, y = price),
#     geom = "point", stat = "identity", position = "identity") +
#   scale_x_continuous() + scale_y_continuous() +
#   coord_cartesian() -> p
# diamonds |> ggplot(aes(x = carat, y = price)) +
#   geom_point() +
#   geom_smooth(method = lm) +
#   scale_y_log10() -> p
# diamonds |> ggplot(aes(x = price)) +
#   geom_bar(aes(y = stat(count)), stat = "bin", bins = 35) -> p
#   geom_bar(aes(y = stat(count)), stat = "bin", breaks = seq(5000, 20000, 5000)) -> p
# diamonds |> ggplot(aes(x = clarity, fill = clarity)) + geom_bar() -> p
# diamonds |> ggplot(aes(x = clarity, fill = clarity)) + geom_bar(width = 1) +
#   coord_polar() -> p
# diamonds |> ggplot(aes(x = "", fill = clarity)) +
#   geom_bar(width = 1) + coord_polar(theta = "y") -> p
# diamonds |> dplyr::filter(y < 3 | y > 20) |> arrange(y)
# diamonds |> ggplot() +
#   geom_histogram(aes(x = y), binwidth = 0.5) +
#   coord_cartesian(ylim = c(0, 50))-> p
# diamonds |> ggplot(aes(x = price, y = stat(density))) +
#   geom_freqpoly(aes(color = cut), binwidth = 500) -> p
# diamonds |> ggplot(aes(x = cut, y = price)) + geom_boxplot() -> p

# faithful |> ggplot(aes(x = eruptions)) + geom_histogram() -> p

# today()
# Sys.Date()
# now()
# Sys.time()
# ymd_hms(20210801123456, tz = "Europe/Madrid")

make_datetime_hhmm <- \(year, month, day, time)
  make_datetime(year, month, day, time %/% 100, time %% 100)

# flights |>
#   dplyr::filter(!is.na(dep_time) & !is.na(arr_time)) |>
#   mutate(
#     dep_time = make_datetime_hhmm(year, month, day, dep_time),
#     arr_time = make_datetime_hhmm(year, month, day, arr_time),
#     sched_dep_time = make_datetime_hhmm(year, month, day, sched_dep_time),
#     sched_arr_time = make_datetime_hhmm(year, month, day, sched_arr_time)) |>
#   select(origin, dest, ends_with("delay"), ends_with("time")) -> flts
# flts |> ggplot(aes(x = dep_time)) + geom_freqpoly(binwidth = 86400) -> p
# flts |> dplyr::filter(dep_time < ymd(20130102)) |>
#   ggplot(aes(dep_time)) + geom_freqpoly(binwidth = 600) -> p
# flts |> mutate(wday = wday(dep_time, label = T)) |>
#   ggplot(aes(x = wday)) + geom_bar() -> p
# flts |> mutate(minute = minute(dep_time)) |> group_by(minute) |>
#   summarize(avg_delay = mean(arr_delay, na.rm = T)) |>
#   ggplot(aes(x = minute, y = avg_delay)) + geom_line() -> p
# flts |> mutate(week = floor_date(dep_time, "week")) |>
#   group_by(week) |> summarize(count = n()) |>
#   ggplot(aes(x = week, y = count)) + geom_line() -> p
# flts |> count(week = floor_date(dep_time, "week")) |>
#   ggplot(aes(x = week, y = n)) + geom_line() -> p
# flts |> mutate(dep_hour = update(dep_time, yday = 1)) |>
#   ggplot(aes(x = dep_hour)) + geom_freqpoly(binwidth = 300) -> p

# flts |>
#   mutate(
#     overnight = arr_time < dep_time,
#     arr_time = arr_time + days(1 * overnight),
#     sched_arr_time = sched_arr_time + days(1 * overnight)) |>
#   dplyr::filter(arr_time < dep_time)

# mtcars |> toJSON(pretty = T)

# j <- '["Amsterdam", "Rotterdam", "Utrecht", "Den Haag"]'
# j |> fromJSON(simplifyVector = F)
# j <- '[{"name":"Erik", "age":43}, {"name":"Anna", "age":32}]'
# j |> fromJSON(simplifyDataFrame = F)
# j <- '[ [1, 2, 3], [4, 5, 6] ]'
# j |> fromJSON(simplifyMatrix = F)

# c(1, 2, NA) |> toJSON(na = "null")
# c(T, F, NA) |> toJSON()
# c("a", "b", NA) |> toJSON()
# c(NaN, Inf, -Inf, NA) |> toJSON()
# (Sys.time() + 1:3) |> toJSON()
# (Sys.Date() + 1:3) |> toJSON()
# factor(c("a", "b", "c")) |> toJSON()

# c(a = 1, b = 2, c = 3) |> toJSON()
# m <- matrix(1:12, nrow = 3)
# colnames(m) <- letters[1:4]
# rownames(m) <- LETTERS[1:3]
# dimnames(m)
# attributes(m)
# length(m)
# m |> toJSON(pretty = T)
# tibble(x = c(1:4) * 1+1i, y = letters[1:4])
# tibble(x = c(1:4) * 1+1i, y = letters[1:4]) |> toJSON(pretty = T)

# total_obra <- 35923.25
# obra_pendiente <- c(1715, 500, 6255, 400, 400, 990, 150)
# total_obra_pendiente <- sum(obra_pendiente)
# total_pagado <- 28079.58
# percentage <- c(
#   total_obra_pendiente = total_obra_pendiente,
#   total_pagado = total_pagado) /
#   total_obra
# percentage

# list(1, "a", T, NA) |> toJSON()
# list(a = 1, b = "a", c = T, d = NA, 10, "A") |> toJSON()
# iris |> tibble()
# iris[1:2, ] |> toJSON(pretty = T)
# (d <- tibble(x = c(1, NA, 2), y = c(NA, "a", "b")))
# d |> toJSON(pretty = T)
# d <- tibble(driver = c("Bowser", "Peach"), occupation = c("Koopa", "Princess"))
# d$vehicle <- tibble(model = c("Piranha Prowler", "Royal Racer"))
# d$vehicle$stats <- tibble(speed = c(55, 34), weight = c(67, 24), drift = c(35, 32))
# str(d)
# d |> toJSON(pretty = T)

# c(1, 2, 3) |> toJSON()
# tibble(x = 1:3, y = letters[1:3]) |> toJSON(pretty = T)
# matrix(1:6, nrow = 2) |> toJSON(pretty = T)
# list(1, "a", T, list(x = 10)) |> toJSON(pretty = T)
# list(x = 1:3, y = letters[1:3]) |> toJSON(pretty = T)

# gss_cat |> ggplot(aes(x = race)) + geom_bar() + scale_x_discrete(drop = F) -> p

# gss_cat |> group_by(race) |> summarize(count = n()) |>
#   ggplot(aes(x = race, y = count)) + geom_col() + scale_x_discrete(drop = F) -> p

# gss_cat |> group_by(relig) |>
#   summarize(tvhours = mean(tvhours, na.rm = T)) |>
#   # ggplot(aes(x = tvhours, y = reorder(relig, tvhours))) + geom_point() -> p
#   mutate(rerelig = reorder(relig, tvhours)) |>
#   ggplot(aes(x = tvhours, y = rerelig)) + geom_point() -> p

# gss_cat |> group_by(rincome) |> summarize(age = mean(age, na.rm = T)) |>
#   ggplot(aes(x = age, y = reorder(rincome, age))) + geom_point() -> p

# gss_cat |> group_by(marital) |> summarize(count = n()) |>
#   ggplot(aes(x = reorder(marital, count), y = count)) + geom_col() -> p

# svglite("plot.svg")
# print(p)
# dev.off()

# flights |> print(n = 20, width = Inf)
# d <- tibble(x = runif(1e2))
# mark(d[d$x > 0.5, ], dplyr::filter(d, x > 0.5)) |> print(width = Inf)
# d
# d$x
# d[["y"]]
# d %>% .$x
# d %>% .[["y"]]

# planes |> group_by(tailnum) |> summarize(m = n()) |> dplyr::filter(m > 1)
# planes |> count(tailnum) |> dplyr::filter(n > 1)

# flts <- flights |> select(year:day, hour, origin, dest, tailnum, carrier)
# flts |> select(-origin, -dest) |> left_join(airlines, by = "carrier")
# flts |> select(-origin, -dest) |>
#   mutate(name = airlines$name[match(carrier, airlines$carrier)])
# flts |> left_join(airports, by = c(dest = "faa"))

# x <- tribble(
#   ~key, ~x,
#   1, "x1",
#   2, "x2",
#   3, "x3",
#   4, "x4")
# y <- tribble(
#   ~key2, ~y,
#   1, "y1",
#   2, "y2",
#   3, "y3",
#   1, "y11")
# x |> inner_join(y, by = c(key = "key2"))

# top_dest <- flights |> group_by(dest) |> summarize(count = n()) |>
#   arrange(desc(count)) |> top_n(10)
# flights |> dplyr::filter(dest %in% top_dest$dest)
# flights |> semi_join(top_dest, by = "dest")
# flights |> anti_join(planes, by = "tailnum") |> count(tailnum) |> arrange(desc(n))
# airports |> count(alt, lon) |> dplyr::filter(n > 1)

# words[str_detect(words, "x$")]
# str_subset(words, "x$")
# d <- tibble(word = words, i = seq_along(words))
# d |> dplyr::filter(str_detect(word, "x$"))

# colors <- c("red", "orange", "yellow", "green", "blue", "purple")
# color_pattern <- str_c(colors, collapse = "|")
# sentences |> str_subset(color_pattern) |> str_extract(color_pattern)
# sentences[str_count(sentences, color_pattern) > 1] |> str_extract_all(color_pattern)
# noun_pattern = "(a|the) ([^ ]+)"
# sentences |> str_subset(noun_pattern) |> str_extract(noun_pattern) |> head(20)
# noun_match <- sentences |> str_subset(noun_pattern) |> str_match(noun_pattern) |> head(20)
# noun_match[, 3]
