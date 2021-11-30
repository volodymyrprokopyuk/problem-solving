library(data.table, warn.conflicts = F)
library(knitr)

locate_benchmarking_data <- \(data_glob)
data_glob |>
  Sys.glob() |>
  strsplit("-")

extract_benchmarking_context <- \(data_path)
list(
  file_path = data_path |> paste(collapse = "-"),
  record_num = data_path[[1]] |> basename(),
  thread_num = data_path[[2]],
  query_name = data_path[[3]] |> tools::file_path_sans_ext()
)

extract_benchmarking_data <- \(bench_context) {
  bench_data <- bench_context$file_path |>
    fread(select = c("timeStamp", "elapsed", "label", "success", "threadName"))
  bench_data[
    ,
    `:=`(
      timeStamp = as.integer(timeStamp %/% 1e3) |>
        as.POSIXct(origin = "1970-01-01", tz = "GMT") + (timeStamp %% 1e3 / 1e3),
      record_num = bench_context$record_num |>
        factor(levels = c("100krecords", "500krecords", "1mrecords")),
      thread_num = bench_context$thread_num |>
        factor(levels = c("1thread", "10thread", "100thread")),
      query_name = bench_context$query_name
    )
  ]
}

process_benchmarking_data <- \(bench_data)
bench_data[label == "Find downstream steps" & success == T]

generate_benchmarking_report <- \(metrics, template, report)
knit(input = template, output = report, envir = rlang::env(metrics = metrics))

"data/*.csv" |>
  locate_benchmarking_data() |>
  purrr::map(extract_benchmarking_context) |>
  purrr::map(extract_benchmarking_data) |>
  rbindlist() |>
  process_benchmarking_data() |>
  generate_benchmarking_report(
    template = "R/ods-benchmarking.Rmd", report = "report/ods-benchmarking.md"
  )
