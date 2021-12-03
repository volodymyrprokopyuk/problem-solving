library(yaml)
library(plumber)

args = commandArgs(trailingOnly = T)
config = read_yaml(args[[1]])

pr("api.R") |>
  pr_run(host = config$server$host, port = config$server$port)
