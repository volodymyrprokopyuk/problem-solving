library(lubridate, warn.conflicts = F)
# library(jsonlite, warn.conflicts = F)
library(knitr)

min_delay = 0.00
max_delay = 0.01

validate_request <- \(payment) {
  print("validate_request")
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

check_account <- \(payment) {
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

check_sanctions <- \(payment) {
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

check_fraud <- \(payment) {
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

process_payment <- \(payment) {
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

book_payment <- \(payment) {
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

submit_payment <- \(payment) {
  print("submit_payment")
  runif(1, min_delay, max_delay) |> Sys.sleep()
  payment
}

# process_credit_transfer <- \(payment)
#   payment |> validate_request() |>
#     check_account() |>
#     check_sanctions() |>
#     check_fraud() |>
#     process_payment() |>
#     book_payment() |>
#     submit_payment()

process_credit_transfer <- \(payment) {
  payment <- validate_request(payment)
  payment <- check_account(payment)
  payment <- check_sanctions(payment)
  payment <- check_fraud(payment)
  payment <- process_payment(payment)
  payment <- book_payment(payment)
  payment <- submit_payment(payment)
}

benchmarking_metrics = NULL

collect_metrics <- \(metrics) {
  if (is.null(benchmarking_metrics)) benchmarking_metrics <<- tibble(!!! metrics)
  else benchmarking_metrics <<- benchmarking_metrics |> add_row(!!! metrics)
}

instrument_action <- \(action, action_name) {
  force(action)
  \(...) {
    start_ts = now()
    result <- action(...)
    end_ts = now()
    list(
      start_ts = start_ts,
      end_ts = end_ts,
      action_name = action_name) |>
      collect_metrics()
    result
  }
}

# instrument_module <- \(module_name) {
instrument_module <- \() {
  validate_request <<- validate_request |>
    instrument_action(action_name = "validate_request")
  check_account <<- check_account |>
    instrument_action(action_name = "check_account")
  check_sanctions <<- check_sanctions |>
    instrument_action(action_name = "check_sanctions")
  check_fraud <<- check_fraud |>
    instrument_action(action_name = "check_fraud")
  process_payment <<- process_payment |>
    instrument_action(action_name = "process_payment")
  book_payment <<- book_payment |>
    instrument_action(action_name = "book_payment")
  submit_payment <<- submit_payment |>
    instrument_action(action_name = "submit_payment")
}

generate_report <- \(template) knit(template)

instrument_module()

process_credit_transfer(1)

generate_report(template = "ipf-benchmarking.Rmd")

# benchmarking_metrics |>
#   mutate(diff = end_ts - start_ts)
#   arrange(start_ts)
