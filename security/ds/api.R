library(yaml)
library(httr)
library(processx)
library(jsonlite)

args = commandArgs(trailingOnly = T)
config = read_yaml(args[[1]])

introspect_token <- \(opaque, policy) {
  introspection_url <- config$authorization$introspection_url
  trusted_issuers <- config$authorization$trusted_issuers
  res <- POST(introspection_url, body = list(token = opaque), encode = "form")
  status <- status_code(res)
  if (status != 200)
    return(list(status = status, error = content(res)))
  token <- content(res)
  if (!token$active)
    return(list(status = 403, error = "Invalid token"))
  if (!any(token$iss %in% trusted_issuers))
    return(list(status = 403, error = "Invalid issuer"))
  if (!all(policy$target_audience %in% token$aud))
    return(list(status = 403, error = "Invalid audience"))
  if (!all(policy$required_scope %in% strsplit(token$scope, " ")[[1]]))
    return(list(status = 403, error = "Invalid scope"))
  list(status = status)
}

verify_jwt <- \(jwt, policy) {
  jwks_url <- config$authorization$jwks_url
  trusted_issuers <- config$authorization$trusted_issuers
  res <- GET(jwks_url)
  status <- status_code(res)
  if (status != 200)
    return(list(status = status, error = content(res)))
  jwks <- content(res, "text")
  p <- run("rnbyc", c("-t", jwt, "-C", "-P", jwks))
  if (p$status != 0 || p$stdout == "")
    return(list(status = 403, error = "Invalid token"))
  token <- fromJSON(p$stdout)
  if (as.numeric(Sys.time()) > token$exp)
    return(list(status = 403, error = "Expired token"))
  if (!any(token$iss %in% trusted_issuers))
    return(list(status = 403, error = "Invalid issuer"))
  if (!all(policy$target_audience %in% token$aud))
    return(list(status = 403, error = "Invalid audience"))
  if (!all(policy$required_scope %in% token$scp))
    return(list(status = 403, error = "Invalid scope"))
  list(status = status)
}

#* Logs request
#* @filter request_log
\(req) {
  cat(req$REQUEST_METHOD, req$PATH_INFO, "\n")
  forward()
}

#* Introspects OAuth2 opaque access token
#* @get /opaque-introspection
#* @serializer unboxedJSON
\(req, res) {
  opaque = sub("(?i)^bearer ", "", req$HTTP_AUTHORIZATION)
  r <- introspect_token(opaque, config$authorization$opaque_introspection)
  if (r$status != 200) {
    res$status <- r$status
    return(list(error = r$error))
  }
  list(data = "Welcome to the data service (opaque)!", token = opaque)
}

#* Verifies OAuth2 JWT access token
#* @get /jwt-verification
#* @serializer unboxedJSON
\(req, res) {
  jwt = sub("(?i)^bearer ", "", req$HTTP_AUTHORIZATION)
  r <- verify_jwt(jwt, config$authorization$jwt_verification)
  if (r$status != 200) {
    res$status <- r$status
    return(list(error = r$error))
  }
  list(data = "Welcome to the data service (JWT)!", token = jwt)
}
