library(rlang, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(purrr, warn.conflicts = F)
library(R6)
library(lubridate, warn.conflicts = F)
library(tibble)
library(dplyr, warn.conflicts = F)
library(knitr)
library(ggplot2)

library(gcookbook)

# Payment processing

payment_action_template <-
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

make_action <- \(min_delay = 0.00, max_delay = 0.01, scale = 1.0)
\(...) runif(1, min_delay * scale, max_delay * scale) |> Sys.sleep()

make_payment_actions <- \() {
  payment_actions <- payment_action_template |>
    map(\(template) make_action(scale = template$scale)) %>%
    env_bind(global_env(), !!!.)
  process_credit_transfer <- \() freduce(NULL, payment_actions)
  env_bind(global_env(), process_credit_transfer = process_credit_transfer)
}

# Application instrumentation

instrument_action <- \(action, metrics_store, ...) {
  force(action)
  dimensions <- list(...)
  \(...) {
    start_ts <- now()
    result <- action(...)
    end_ts <- now()
    c(list(start_ts = start_ts, end_ts = end_ts), dimensions) |>
      metrics_store$collect_metrics()
    result
  }
}

instrument_application <- \(metrics_store, app_name, benchmark_name, iteration_name) {
  payment_actions <- payment_action_template |>
    map(\(template) {
      instrument_action(
        env_get(global_env(), nm = template$action_name, default = NULL),
        metrics_store,
        app_name = app_name,
        benchmark_name = benchmark_name,
        iteration_name = iteration_name,
        module_name = template$module_name,
        action_name = template$action_name
      )
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

process_metrics <- \(metrics)
metrics |>
  mutate(module_name = ordered(module_name, levels = c(
    "payment_validation", "payment_processing"
  ))) |>
  mutate(action_name = ordered(action_name, levels = c(
    "validate_request", "check_account", "check_sanctions", "check_fraud",
    "process_payment", "book_payment", "submit_payment"
  ))) |>
  mutate(elapsed_time = end_ts - start_ts)

# Benchmarking execution

execute_benchmark <- \(metrics_store, runs = 5, iterations = 30, app_name) {
  walk(1:runs, \(run_number) {
    walk(1:iterations, \(iteration_number) {
      instrument_application(
        metrics_store = metrics_store,
        app_name = app_name,
        benchmark_name = sprintf("Benchmark %04.f", run_number),
        iteration_name = sprintf("Iteraiton %04.f", iteration_number)
      )
      process_credit_transfer()
    })
  })
  metrics_store$metrics
}

# Benchmarking reporting

generate_report <- \(metrics, template) knit(template, envir = env(metrics = metrics))


make_payment_actions()
MetricsStore$new() |>
  execute_benchmark(runs = 1, iterations = 20, app_name = "credit_transfer") |>
  process_metrics() |>
  generate_report(template = "ipf-benchmarking.Rmd")
