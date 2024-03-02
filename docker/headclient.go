// go build headclient.go
// HEAD_HOST=https://github.com ./headclient
package main

import (
  "fmt"
  "time"
  "os"
  "net/http"
)

func main() {
  host := os.Getenv("HEAD_HOST")
  for {
    res, err := http.Head(host)
    if err != nil {
      fmt.Println(err)
      time.Sleep(1000 * time.Millisecond)
      continue
    }
    server, _ := res.Header["Server"]
    fmt.Printf("%v %v %v\n", time.Now().Format("15:04:05"), res.Status, server)
    time.Sleep(5000 * time.Millisecond)
  }
}
