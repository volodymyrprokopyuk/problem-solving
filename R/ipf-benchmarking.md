---
title: IPF benchmarking
---


```r
opts_chunk$set(dev = "svglite")
```


```r
metrics |>
  ggplot(aes(x = action_name, y = elapsed_time)) + geom_boxplot()
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

![plot of chunk benchmarking-metrics](figure/benchmarking-metrics-1.svg)

```r
metrics |>
  group_by(action_name) |> summarize(elapsed_time = mean(elapsed_time)) |>
  ggplot(aes(x = action_name, y = elapsed_time)) + geom_col()
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

![plot of chunk benchmarking-metrics](figure/benchmarking-metrics-2.svg)

```r
metrics |>
  group_by(module_name, action_name) |> summarize(elapsed_time = mean(elapsed_time)) |>
  ggplot(aes(x = action_name, y = elapsed_time, fill = module_name)) + geom_col()
```

```
## `summarise()` has grouped output by 'module_name'. You can override using the `.groups` argument.
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

![plot of chunk benchmarking-metrics](figure/benchmarking-metrics-3.svg)
