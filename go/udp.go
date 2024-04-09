package main

import (
	"fmt"
	"net"
	"os"
)

const (
  server = "127.0.0.1:7654"
  client = "127.0.0.1:7655"
)

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func listenServer() {
  conn, err := net.ListenPacket("udp", server)
  exitOnError(err)
  defer conn.Close()
  fmt.Printf("listening %v\n", conn.LocalAddr())
  buf := make([]byte, 128)
  for {
    n, fromAddr, err := conn.ReadFrom(buf)
    if err != nil {
      fmt.Println(err)
      continue
    }
    fmt.Printf("%v: %v\n", fromAddr, string(buf[:n]))
    _, err = conn.WriteTo(buf[:n], fromAddr)
    if err != nil {
      fmt.Println(err)
      continue
    }
  }
}

func listenClient() {
  conn, err := net.ListenPacket("udp", client)
  exitOnError(err)
  defer conn.Close()
  toAddr, err := net.ResolveUDPAddr("udp", server)
  exitOnError(err)
  _, err = conn.WriteTo([]byte("echo"), toAddr)
  exitOnError(err)
  buf := make([]byte, 128)
  n, fromAddr, err := conn.ReadFrom(buf)
  exitOnError(err)
  fmt.Printf("%v: %v\n", fromAddr, string(buf[:n]))
}

func listenClientTCP() {
  dial, err := net.Dial("udp", server)
  exitOnError(err)
  defer dial.Close()
  _, err = dial.Write([]byte("echo"))
  exitOnError(err)
  buf := make([]byte, 128)
  n, err := dial.Read(buf)
  exitOnError(err)
  fmt.Printf("%v\n", string(buf[:n]))
}

func main() {
  if len(os.Args) > 1 && os.Args[1] == "-l" {
    listenServer()
    return
  }
  // listenClient()
  listenClientTCP()
}
