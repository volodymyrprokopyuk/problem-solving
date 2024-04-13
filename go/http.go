package main

import (
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

const (
  server = "127.0.0.1:7654"
  serverTLS = "localhost:7654"
)

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func log(next http.Handler) http.Handler {
  return http.HandlerFunc(func (w http.ResponseWriter, r *http.Request) {
    fmt.Printf("%v %v\n", r.Method, r.URL.Path)
    next.ServeHTTP(w, r)
  })
}

func getDate(w http.ResponseWriter, r *http.Request) {
  w.Header().Set("Content-Type", "text/plain")
  w.WriteHeader(http.StatusOK)
  fmt.Fprintf(w, "%v\n", time.Now().Format(time.DateTime))
}

type Counter struct {
  Value int `json:"value"`
}

func postCounter(w http.ResponseWriter, r *http.Request) {
  var cnt Counter
  err := json.NewDecoder(r.Body).Decode(&cnt)
  if err != nil {
    http.Error(w, err.Error(), http.StatusBadRequest)
    return
  }
  cnt.Value++
  w.Header().Set("Content-Type", "application/json")
  w.WriteHeader(http.StatusCreated)
  err = json.NewEncoder(w).Encode(&cnt)
  if err != nil {
    http.Error(w, err.Error(), http.StatusInternalServerError)
    return
  }
}

func createMux() *http.ServeMux {
  mux := http.NewServeMux()
  mux.HandleFunc("GET /date", getDate)
  mux.Handle("POST /counter", http.HandlerFunc(postCounter))
  return mux
}

func listen() {
  mux := createMux()
  fmt.Printf("listening %v\n", server)
  err := http.ListenAndServe(server, log(mux))
  exitOnError(err)
}

func listenTLS() {
  mux := createMux()
  fmt.Printf("listening %v\n", serverTLS)
  err := http.ListenAndServeTLS(serverTLS, "srvcert.pem", "srvkey.pem", log(mux))
  exitOnError(err)
}

func listenServer() {
  mux := createMux()
  srv := &http.Server{
    Addr: server,
    Handler: log(mux),
  }
  fmt.Printf("listening %v\n", server)
  err := srv.ListenAndServe()
  exitOnError(err)
}

func listenServerTLS() {
  mux := createMux()
  srv := &http.Server{
    Addr: server,
    Handler: log(mux),
  }
  fmt.Printf("listening %v\n", serverTLS)
  err := srv.ListenAndServeTLS("srvcert.pem", "srvkey.pem")
  exitOnError(err)
}

func listenServermTLS() {
  mux := createMux()
  cacert, err := os.ReadFile("cacert.pem")
  exitOnError(err)
  certpool := x509.NewCertPool()
  certpool.AppendCertsFromPEM(cacert)
  srv := &http.Server{
    Addr: serverTLS,
    Handler: log(mux),
    TLSConfig: &tls.Config{
      ClientAuth: tls.RequireAndVerifyClientCert,
      ClientCAs: certpool,
    },
  }
  fmt.Printf("listening %v\n", serverTLS)
  err = srv.ListenAndServeTLS("srvcert.pem", "srvkey.pem")
  exitOnError(err)
}

func dateGet() {
  res, err := http.Get(fmt.Sprintf("http://%v/date", server))
  exitOnError(err)
  defer res.Body.Close()
  fmt.Printf("%v\n", res.Header.Get("Content-Type"))
  io.Copy(os.Stdout, res.Body)
}

func dateGetCtx() {
  ctx, cancel := context.WithTimeout(context.Background(), 1e3 * time.Millisecond)
  defer cancel()
  req, err := http.NewRequestWithContext(
    ctx, http.MethodGet, fmt.Sprintf("http://%v/date", server), nil,
  )
  exitOnError(err)
  var cln http.Client
  res, err := cln.Do(req)
  exitOnError(err)
  defer res.Body.Close()
  fmt.Printf("%v\n", res.Header.Get("Content-Type"))
  io.Copy(os.Stdout, res.Body)
}

func processCounterPost(cln *http.Client, url string) {
  cnt := Counter{Value: 1}
  var buf bytes.Buffer
  err := json.NewEncoder(&buf).Encode(&cnt)
  exitOnError(err)
  res, err := cln.Post(url, "application/json", &buf)
  exitOnError(err)
  defer res.Body.Close()
  err = json.NewDecoder(res.Body).Decode(&cnt)
  exitOnError(err)
  fmt.Printf("%+v\n", cnt)
}

func counterPost() {
  var cln http.Client
  processCounterPost(&cln, fmt.Sprintf("http://%v/counter", server))
}

func counterPostTLS() {
  cacert, err := os.ReadFile("cacert.pem")
  exitOnError(err)
  certpool := x509.NewCertPool()
  certpool.AppendCertsFromPEM(cacert)
  cln := http.Client{
    Transport: &http.Transport{
      TLSClientConfig: &tls.Config{
        RootCAs: certpool,
      },
    },
  }
  processCounterPost(&cln, fmt.Sprintf("https://%v/counter", serverTLS))
}

func counterPostmTLS() {
  cacert, err := os.ReadFile("cacert.pem")
  exitOnError(err)
  certpool := x509.NewCertPool()
  certpool.AppendCertsFromPEM(cacert)
  clncert, err := tls.LoadX509KeyPair("clncert.pem", "clnkey.pem")
  exitOnError(err)
  cln := http.Client{
    Transport: &http.Transport{
      TLSClientConfig: &tls.Config{
        RootCAs: certpool,
        Certificates: []tls.Certificate{clncert},
      },
    },
  }
  processCounterPost(&cln, fmt.Sprintf("https://%v/counter", serverTLS))
}

func main() {
  if len(os.Args) > 1 && os.Args[1] == "-l" {
    // listen()
    // listenTLS()
    // listenServer()
    // curl https://localhost:7654/counter -d '{"value":1}'
    // listenServerTLS()
    // curl https://localhost:7654/counter -d '{"value":1}' --cacert cacert.pem
    listenServermTLS()
    // curl https://localhost:7654/counter -d '{"value":1}' --cacert cacert.pem \
    //   --cert clncert.pem --key clnkey.pem
  }
  // dateGet()
  // dateGetCtx()
  // counterPost()
  // counterPostTLS()
  counterPostmTLS()
}
