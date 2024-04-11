package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"time"
)

const server = "127.0.0.1:7654"

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

func listen() {
  mux := http.NewServeMux()
  mux.HandleFunc("GET /date", getDate)
  // mux.HandleFunc("POST /counter",postCounter)
  mux.Handle("POST /counter", http.HandlerFunc(postCounter))
  fmt.Printf("listening %v\n", server)
  err := http.ListenAndServe(server, log(mux))
  exitOnError(err)
}

func listenServer() {
  mux := http.NewServeMux()
  mux.HandleFunc("GET /date", getDate)
  mux.HandleFunc("POST /counter",postCounter)
  srv := &http.Server{Addr: server, Handler: log(mux)}
  lis, err := net.Listen("tcp", srv.Addr)
  exitOnError(err)
  fmt.Printf("listening %v\n", server)
  err = srv.Serve(lis)
  // err = srv.ServeTLS(lis, "srvcert.pem", "srvkey.pem")
  exitOnError(err)
}

func dateHead() {
  res, err := http.Head(fmt.Sprintf("http://%v/date", server))
  exitOnError(err)
  defer res.Body.Close()
  fmt.Println(res.Header.Get("Content-Type"))
}

func dateGetCtx() {
  ctx, cancel := context.WithTimeout(context.Background(), 1000 * time.Millisecond)
  defer cancel()
  req, err := http.NewRequestWithContext(
    ctx, http.MethodGet, fmt.Sprintf("http://%v/date", server), nil,
  )
  exitOnError(err)
  res, err := http.DefaultClient.Do(req)
  exitOnError(err)
  defer res.Body.Close()
  io.Copy(os.Stdout, res.Body)
}

func counterPost() {
  cnt := Counter{Value: 1}
  var buf bytes.Buffer
  err := json.NewEncoder(&buf).Encode(&cnt)
  exitOnError(err)
  res, err := http.Post(
    fmt.Sprintf("http://%v/counter", server), "application/json", &buf,
  )
  exitOnError(err)
  defer res.Body.Close()
  err = json.NewDecoder(res.Body).Decode(&cnt)
  fmt.Printf("%+v\n", cnt)
}

func main() {
  if len(os.Args) > 1 && os.Args[1] == "-l" {
    // listen()
    listenServer()
  }
  dateHead()
  dateGetCtx()
  counterPost()
}
