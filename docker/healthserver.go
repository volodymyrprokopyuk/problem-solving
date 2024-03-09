package main

import (
  "fmt"
  "time"
  "math/rand"
  "net/http"
  // "crypto/tls"
)

func logging(next http.Handler) http.Handler {
  handler := func(w http.ResponseWriter, r *http.Request) {
    start := time.Now()
    next.ServeHTTP(w, r)
    fmt.Printf("%v %v %v\n", r.Method, r.RequestURI, time.Since(start))
  }
  return http.HandlerFunc(handler)
}

func random(w http.ResponseWriter, r *http.Request) {
  fmt.Fprintf(w, "%v\n", rand.Intn(1000))
}

var healthCount = 0

func health(w http.ResponseWriter, r *http.Request) {
  healthCount++
  if healthCount > 3 {
    code := http.StatusServiceUnavailable
    http.Error(w, http.StatusText(code), code)
    return
  }
  fmt.Fprintf(w, "Healthy\n")
}

func main() {
  mux := http.NewServeMux()
  mux.HandleFunc("GET /random", random)
  mux.HandleFunc("GET /health", health)
  http.ListenAndServe(":4321", logging(mux))
}

// func main() {
//   mux := http.NewServeMux()
//   mux.Handle("GET /random", logging(http.HandlerFunc(random)))
//   server := &http.Server{
//     Addr: ":7512", Handler: mux,
//     TLSConfig: &tls.Config{
//       MinVersion: tls.VersionTLS13,
//       PreferServerCipherSuites: true,
//     },
//   }
//   err := server.ListenAndServeTLS("cert.pem", "key.pem")
//   if err != nil {
//     fmt.Println(err)
//   }
// }
