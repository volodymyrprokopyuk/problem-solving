package main

import (
	"context"
	"fmt"
	"net"
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

func listen() {
  lis, err := net.Listen("tcp", server)
  exitOnError(err)
  defer lis.Close()
  fmt.Printf("listening %v\n", lis.Addr())

  for {
    conn, err := lis.Accept()
    if err != nil {
      fmt.Println(err)
      continue
    }
    go func(conn net.Conn) {
      defer conn.Close()
      buf := make([]byte, 128)
      for {
        n, err := conn.Read(buf)
        if err != nil {
          fmt.Println(err)
          return
        }
        fmt.Printf("%v: %v\n", conn.RemoteAddr(), string(buf[:n]))
        _, err = conn.Write([]byte("pong"))
        if err != nil {
          fmt.Println(err)
          return
        }
      }
    }(conn)
  }
}

func dial() {
  conn, err := net.Dial("tcp", server)
  exitOnError(err)
  defer conn.Close()
  _, err = conn.Write([]byte("ping"))
  exitOnError(err)
  buf := make([]byte, 128)
  n, err := conn.Read(buf)
  exitOnError(err)
  fmt.Printf("%v\n", string(buf[:n]))
}

func dialTimeout() {
  ctx, cancel := context.WithTimeout(context.Background(), 1e4 * time.Microsecond)
  defer cancel()
  var dial net.Dialer
  conn, err := dial.DialContext(ctx, "tcp", server)
  exitOnError(err)
  defer conn.Close()
  _, err = conn.Write([]byte("ping"))
  exitOnError(err)
  buf := make([]byte, 128)
  n, err := conn.Read(buf)
  exitOnError(err)
  fmt.Printf("%v\n", string(buf[:n]))
}

func dialHeartbeat() {
  ctx, cancel := context.WithCancel(context.Background())
  defer cancel()
  var dial net.Dialer
  conn, err := dial.DialContext(ctx, "tcp", server)
  exitOnError(err)
  defer conn.Close()
  go func() {
    tick := time.NewTicker(800 * time.Millisecond)
    defer tick.Stop()
    for {
      select {
      case <- ctx.Done():
        fmt.Println("done")
        return
      case <- tick.C:
        _, err := conn.Write([]byte("ping"))
        if err != nil {
          fmt.Println(err)
        }
      }
    }
  }()
  for range 5 {
    buf := make([]byte, 128)
    n, err := conn.Read(buf)
    exitOnError(err)
    fmt.Printf("%v\n", string(buf[:n]))
  }
}

func main() {
  if len(os.Args) > 1 && os.Args[1] == "-l" {
    listen()
    return
  }
  // dial()
  // dialTimeout()
  dialHeartbeat()
}
