library(clock)
library(jsonlite)
library(data.table)
library(ggplot2)
library(plotly, warn.conflicts = F)

#* Echoes back the input
#* @get /echo
#* @param msg
# \(req, msg = "") c(msg = msg, username = req$username)
# \(req, msg = "") list(msg = msg, username = req$username)
# \(req, msg = "") data.table(msg = msg, username = req$username)
# \(req, msg = "")
# list(
#   vector = c(msg = msg, username = req$username),
#   list = list(msg = msg, username = req$username),
#   data.table = data.table(msg = msg, username = req$username),
#   msg = unbox(msg)
# )
\(req, msg = "")
list(msg = msg, argsQuery = req$argsQuery, QS = req$QUERY_STRING)

#* Plots a histogram
#* @get /plot
#* @serializer png
\() rnorm(100) |> hist()

#* Returns the sum of two numbers
#* @post /sum
#* @param a
#* @param b
\(a, b) as.numeric(a) + as.numeric(b)

#* Filters and plots the iris data set
#* @get /iris
#* @param spec
#* @serializer htmlwidget
# @serializer contentType list(type = "image/svg+xml")
\(spec) {
  d <- data.table(iris)
  if (!missing(spec)) d <- d[Species == spec]
  p <- d |> ggplot(aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
    geom_point()
  ggplotly(p)

  # tmp <- tempfile()
  # svg(tmp)
  # print(p)
  # dev.off()
  # readBin(tmp, "raw", n = file.info(tmp)$size)
}

#* Returns HTML
#* @get /html
#* @serializer html
\() "<html>
  <head><title>Minimal HTML</title></head>
  <body><h1>Minimal HTML</h1></body>
</html>"

#* Logs incoming requests
#* @filter log_request
\(req) {
  paste(
    date_now("UTC"), req$REQUEST_METHOD, req$PATH_INFO,
    req$HTTP_USER_AGENT, req$REMOTE_ADDR, "\n"
  ) |> cat()
  forward()
}

#* Sets user
#* @filter set_user
\(req) {
  req$username <- "anonymous"
  forward()
}

#* Returns user by id
#* @get /users/<id>
\(id) list(user_id = unbox(id))

#* Static file server
#* @assets . /assets
list()

#* Returns submitted user
#* @post /users
#* serializer json list(auto_unbox = T)
#* @serializer unboxedJSON
\(req, id, name)
list(id = id, name = I(name), body = req$body, raw = req$bodyRaw, http_host = req$HTTP_HOST)

#* Raises an error
#* @get /error
# \() stop("oh")
\(res) {
  res$status = 404
  list(error = "Resource not found")
}
