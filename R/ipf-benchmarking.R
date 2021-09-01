library(rlang, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(purrr, warn.conflicts = F)
library(R6)
library(lubridate, warn.conflicts = F)
library(tibble)
library(dplyr, warn.conflicts = F)
library(jsonlite, warn.conflicts = F)
library(knitr)
library(ggplot2)
library(readr)

library(gcookbook)
library(MASS, warn.conflicts = F)
library(grid)

# Payment processing

payment_actions_template <-
  list(
    validate_request = list(
      scale = 1.0, action_name = "validate_request", module_name = "payment_validation"
    ),
    check_account = list(
      scale = 2.0, action_name = "check_account", module_name = "payment_validation"
    ),
    check_sanctions = list(
      scale = 1.0, action_name = "check_sanctions", module_name = "payment_validation"
    ),
    check_fraud = list(
      scale = 1.0, action_name = "check_fraud", module_name = "payment_validation"
    ),
    process_payment = list(
      scale = 3.0, action_name = "process_payment", module_name = "payment_processing"
    ),
    book_payment = list(
      scale = 4.0, action_name = "book_payment", module_name = "payment_processing"
    ),
    submit_payment = list(
      scale = 1.0, action_name = "submit_payment", module_name = "payment_processing"
    )
  )

make_action <- \(min_delay = 0.001, max_delay = 0.005, scale = 1.0)
\(...) runif(1, min_delay * scale, max_delay * scale) |> Sys.sleep()

make_payment_actions <- \() {
  payment_actions <- payment_actions_template |>
    map(\(action) make_action(scale = action$scale)) %>%
    env_bind(global_env(), !!!.)
  process_credit_transfer <- \() freduce(NULL, payment_actions)
  env_bind(global_env(), process_credit_transfer = process_credit_transfer)
}

# Application instrumentation

instrument_action <- \(action, dimensions, metrics_store) {
  force(action)
  \(...) {
    start_ts <- now()
    result <- action(...)
    end_ts <- now()
    c(list(start_ts = start_ts, end_ts = end_ts), dimensions) |>
      metrics_store$collect_metrics()
    result
  }
}

instrument_application <- \(..., metrics_store) {
  payment_actions <- payment_actions_template |>
    map(\(template) {
      action <- env_get(global_env(), nm = template$action_name, default = NULL)
      dimensions <- c(
        list(...),
        module_name = template$module_name,
        action_name = template$action_name
      )
      instrument_action(action, dimensions, metrics_store)
    })
  process_credit_transfer <- \() freduce(NULL, payment_actions)
  env_bind(global_env(), process_credit_transfer = process_credit_transfer)
}

# Benchmarking metrics

MetricsStore <-
  R6Class(
    "MetricsStore",
    public = list(
      collect_metrics = \(metrics)
      if (is.null(private$data)) {
        private$data <- tibble(!!!metrics)
      } else {
        private$data <- private$data |> add_row(!!!metrics)
      }
    ),
    active = list(
      metrics = \(metrics)
      if (missing(metrics)) private$data else private$data <- metrics
    ),
    private = list(
      data = NULL
    )
  )

export_metrics <- \(metrics, sink)
metrics |>
  toJSON(pretty = T) |>
  write_file(file = sink)

import_metrics <- \(source)
source |>
  read_file() |>
  fromJSON() |>
  tibble() |>
  mutate(start_ts = ymd_hms(start_ts), end_ts = ymd_hms(end_ts))

process_metrics <- \(metrics)
metrics |>
  mutate(module_name = factor(module_name, levels = c(
    "payment_validation", "payment_processing"
  ))) |>
  mutate(action_name = factor(action_name, levels = c(
    "validate_request", "check_account", "check_sanctions", "check_fraud",
    "process_payment", "book_payment", "submit_payment"
  ))) |>
  mutate(elapsed_time = end_ts - start_ts)

# Benchmarking execution

execute_benchmark <- \(app_name, runs = 5, iterations = 30, metrics_store) {
  walk(1:runs, \(run_number) {
    walk(1:iterations, \(iteration_number) {
      instrument_application(
        app_name = app_name,
        benchmark_name = sprintf("benchmark_%04.f", run_number),
        iteration_name = sprintf("iteraiton_%04.f", iteration_number),
        metrics_store = metrics_store
      )
      process_credit_transfer()
    })
  })
  metrics_store$metrics
}

# Benchmarking reporting

generate_report <- \(metrics, template)
knit(template, envir = env(metrics = metrics))

# Benchmarking management

# make_payment_actions()

# execute_benchmark(
#   app_name = "credit_transfer",
#   runs = 10, iterations = 10,
#   metrics_store = MetricsStore$new()) |>
#   export_metrics(sink = "data/ipf-benchmarking.json")

  import_metrics(source = "data/ipf-benchmarking.json") |>
  process_metrics() |>
  generate_report(template = "ipf-benchmarking.Rmd")



# d <- tribble(
#   ~bench, ~iter, ~module, ~action, ~etime,
#   "1", "01", "validate", "check", 1,
#   "1", "01", "process", "book", 2,
#   "1", "01", "process", "submit", 3,
#   "1", "02", "validate", "check", 4,
#   "1", "02", "process", "book", 5,
#   "1", "02", "process", "submit", 6,
#   "2", "01", "validate", "check", 10,
#   "2", "01", "process", "book", 20,
#   "2", "01", "process", "submit", 30,
#   "2", "02", "validate", "check", 40,
#   "2", "02", "process", "book", 50,
#   "2", "02", "process", "submit", 60,
#   ) |>
#   mutate(
#     module = factor(module, levels = c("validate", "process")),
#     action = factor(action, levels = c("check", "book", "submit"))
#   )

# d |> group_by(bench, iter, module) |> summarize(
#   avg_time = mean(etime), min_time = min(etime), max_time = max(etime)
# )
# d |> group_by(bench, iter, module) |> mutate(
#   avg_time = mean(etime), min_time = min(etime), max_time = max(etime)
# )
# d |> group_by (action) |> mutate(
#   avg_time = mean(etime), min_time = min(etime), max_time = max(etime)
# )
# d |> group_by (bench, iter, module) |> mutate(total_time = sum(etime))
