package main

import (
  "fmt"
  "slices"
  "net/http"
  "time"
  "runtime/debug"
)

type HTTPServer struct { }

// * implements the http.Handler interface
func (hs *HTTPServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  fmt.Fprintf(w, "ok\n")
}

type LogMiddleware struct {
  handler http.Handler
}

// composable middleware implements the same http.Handler interface
func (mw *LogMiddleware) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  // pre-processing
  start := time.Now()
  // handler forwarding
  mw.handler.ServeHTTP(w, r)
  // post-processing
  fmt.Printf("%s %s %s\n", r.Method, r.RequestURI, time.Since(start))
}

// func main() {
//   var server HTTPServer
//   // wrap a server handler into a middleware => a chain of http.Handler
//   logger := LogMiddleware{handler: &server}
//   http.ListenAndServe(":7512", &logger)
// }



func httpHandler(w http.ResponseWriter, r *http.Request) {
  if r.URL.Path == "/panic" {
    panic("oh")
  }
  fmt.Fprintf(w, "ok\n")
}

// * middleware signature takes an http.Handler and returns an http.Handler
func logMiddleware(next http.Handler) http.Handler {
  handler := func(w http.ResponseWriter, r *http.Request) {
    start := time.Now()
    next.ServeHTTP(w, r)
    fmt.Printf("%s %s %s\n", r.Method, r.RequestURI, time.Since(start))
  }
  return http.HandlerFunc(handler)
}

func panicRecover(next http.Handler) http.Handler {
  handler := func(w http.ResponseWriter, r *http.Request) {
    defer func() {
      if err := recover(); err != nil {
        code := http.StatusInternalServerError
        http.Error(w, http.StatusText(code), code)
        fmt.Println(string(debug.Stack()))
      }
    }()
    next.ServeHTTP(w, r)
  }
  return http.HandlerFunc(handler)
}

// func main() {
//   // the http.HandlerFunc type implements the http.Handler interface
//   server := http.HandlerFunc(httpHandler)
//   logger := logMiddleware(server)
//   recover := panicRecover(logger)
//   // middleware chain: recover => log -> serve
//   http.ListenAndServe(":7512", recover)
// }



// * middleware signature takes an http.Handler and returns an http.Handler
type MW func(http.Handler) http.Handler

func Middleware(mws ...MW) MW {
  slices.Reverse(mws)
  return func(next http.Handler) http.Handler {
    for _, mw := range mws {
      next = mw(next)
    }
    return next
  }
}

func main() {
  // the http.HandlerFunc type implements the http.Handler interface
  server := http.HandlerFunc(httpHandler)
  mw := Middleware(panicRecover, logMiddleware)
  // middleware chain: recover => log -> serve
  http.ListenAndServe(":7512", mw(server))
}
