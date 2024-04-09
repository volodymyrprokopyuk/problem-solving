package main

import (
	"fmt"
	"io"
	"net"
	"os"
	"os/signal"
)

const socket = "server.sock"

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func cleanupSocket() {
  sig := make(chan os.Signal, 1)
  signal.Notify(sig, os.Interrupt)
  go func() {
    <- sig
    os.Remove(socket)
    os.Exit(1)
  }()
}

func listenSocket() {
  lis, err := net.Listen("unix", socket)
  exitOnError(err)
  fmt.Printf("listening %v\n", socket)
  for {
    conn, err := lis.Accept()
    if err != nil {
      fmt.Println(err)
      continue
    }
    go func(conn net.Conn) {
      defer conn.Close()
      for {
        buf := make([]byte, 128)
        n, err := conn.Read(buf)
        if err != nil {
          if err == io.EOF {
            break
          }
          fmt.Println(err)
          continue
        }
        fmt.Printf("%v\n", string(buf[:n]))
        _, err = conn.Write(buf[:n])
        if err != nil {
          fmt.Println(err)
          continue
        }
      }
    }(conn)
  }
}

func dialSocket() {
  conn, err := net.Dial("unix", socket)
  exitOnError(err)
  defer conn.Close()
  _, err = conn.Write([]byte("echo"))
  exitOnError(err)
  buf := make([]byte, 128)
  n, err := conn.Read(buf)
  exitOnError(err)
  fmt.Printf("%v\n", string(buf[:n]))
}

func main() {
  if len(os.Args) > 1 && os.Args[1] == "-l" {
    cleanupSocket()
    listenSocket()
  }
  dialSocket()
}
