library(rlang, warn.conflicts = F)
library(R6)
library(lubridate, warn.conflicts = F)
library(knitr)

# Payment processing

make_action <- \(min_delay = 0.00, max_delay = 0.01, scale = 1.0)
  \() runif(1, min_delay * scale, max_delay * scale) |> Sys.sleep()

validate_request <- make_action()
check_account <- make_action(scale = 2)
check_sanctions <- make_action()
check_fraud <- make_action()
process_payment <- make_action(scale = 3)
book_payment <- make_action(scale = 4)
submit_payment <- make_action()

process_credit_transfer <- \() {
  validate_request()
  check_account()
  check_sanctions()
  check_fraud()
  process_payment()
  book_payment()
  submit_payment()
}

# Application instrumentation

instrument_action <- \(action, metrics_store, ...) {
  force(action)
  dimensions <- list(...)
  \(...) {
    start_ts = now()
    result <- action(...)
    end_ts = now()
    c(list(start_ts = start_ts, end_ts = end_ts), dimensions) |>
      metrics_store$collect_metrics()
    result
  }
}

instrument_application <- \(app_name, metrics_store) {
  validate_request <<- validate_request |>
    instrument_action(
      action_name = "validate_request",
      module_name = "payment_validation",
      app_name = app_name,
      metrics_store = metrics_store)
  check_account <<- check_account |>
    instrument_action(
      action_name = "check_account",
      module_name = "payment_validation",
      app_name = app_name,
      metrics_store = metrics_store)
  check_sanctions <<- check_sanctions |>
    instrument_action(
      action_name = "check_sanctions",
      module_name = "payment_validation",
      app_name = app_name,
      metrics_store = metrics_store)
  check_fraud <<- check_fraud |>
    instrument_action(
      action_name = "check_fraud",
      module_name = "payment_validation",
      app_name = app_name,
      metrics_store = metrics_store)
  process_payment <<- process_payment |>
    instrument_action(
      action_name = "process_payment",
      module_name = "payment_processing",
      app_name = app_name,
      metrics_store = metrics_store)
  book_payment <<- book_payment |>
    instrument_action(
      action_name = "book_payment",
      module_name = "payment_processing",
      app_name = app_name,
      metrics_store = metrics_store)
  submit_payment <<- submit_payment |>
    instrument_action(
      action_name = "submit_payment",
      module_name = "payment_processing",
      app_name = app_name,
      metrics_store = metrics_store)
}

# Benchmarking metrics

MetricsStore <-
  R6Class(
    "MetricsStore",
    public = list(
      collect_metrics = \(metrics)
        if (is.null(private$data)) private$data <- tibble(!!! metrics)
        else private$data <- private$data |> add_row(!!! metrics)),
    active = list(
      metrics = \(metrics)
        if (missing(metrics)) private$data else private$data <- metrics),
    private = list(
      data = NULL))

process_metrics <- \(metrics)
  metrics |>
    mutate(action_name = ordered(action_name, levels = c(
      "validate_request", "check_account", "check_sanctions", "check_fraud",
      "process_payment", "book_payment", "submit_payment"))) |>
    mutate(elapsed_time = end_ts - start_ts)

# Benchmarking execution

execute_benchmarking <- \(times = 30) walk(1:times, \(...) process_credit_transfer())

# Benchmarking reporting

generate_report <- \(metrics, template) knit(template, envir = env(metrics = metrics))


metrics_store <- MetricsStore$new()
instrument_application(app_name = "credit_transfer", metrics_store = metrics_store)
execute_benchmarking(times = 30)
metrics_store$metrics |> process_metrics() |>
  generate_report(template = "ipf-benchmarking.Rmd")
