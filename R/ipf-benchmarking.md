---
title: IPF benchmarking
---


```r
opts_chunk$set(dev = "svglite")
```


```r
benchmarking_metrics |> #arrange(start_ts) |>
  ggplot(aes(x = reorder(action_name, desc(start_ts)))) +
  geom_line(aes(y = start_ts), group = 1)
```

![plot of chunk benchmarking-metrics](figure/benchmarking-metrics-1.svg)
