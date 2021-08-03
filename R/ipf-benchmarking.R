# library(ggplot2)
# library(bench)

min_delay = 0.1
max_delay = 0.5

validate_request <- \(payment) {
  1 |> runif(min = min_delay, max = max_delay) |> Sys.sleep()
  payment
}

check_account <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

check_sanctions <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

check_fraud <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

process_payment <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

book_payment <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

submit_payment <- \(payment) {
  Sys.sleep(runif(1, min = min_delay, max = max_delay))
  payment
}

process_credit_transfer <- \(payment)
  payment |> validate_request() |>
    check_account() |>
    check_sanctions() |>
    check_fraud() |>
    process_payment() |>
    book_payment() |>
    submit_payment()

# (start_time <- Sys.time())
# proc.time()
# process_credit_transfer(1)
# proc.time()
# (stop_time <- Sys.time())
# stop_time - start_time

# svg("plot.svg")
# print(p)
# dev.off()
