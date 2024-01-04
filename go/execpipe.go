package main

import (
  "fmt"
  "strings"
  "os/exec"
)

func useStdinStdout() {
  cmd := exec.Command("tr", "a-z", "A-Z")
  cmd.Stdin = strings.NewReader("ok") // io.Reader
  var out strings.Builder // io.Writer
  cmd.Stdout = &out
  err := cmd.Run() // blocks
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(out.String()) // OK
}

func useGoStdinPipe() {
  cmd := exec.Command("tr", "a-z", "A-Z")
  stdin, err := cmd.StdinPipe()
  if err != nil {
    fmt.Println(err)
    return
  }
  go func() { // must be goroutine
    defer stdin.Close()
    stdin.Write([]byte("ok"))
  }()
  out, err := cmd.Output() // blocks
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(string(out))
}

func useAsyncStdinPipe() {
  cmd := exec.Command("tr", "a-z", "A-Z")
  stdin, err := cmd.StdinPipe()
  if err != nil {
    fmt.Println(err)
    return
  }
  var out strings.Builder
  cmd.Stdout = &out
  err = cmd.Start() // async
  if err != nil {
    fmt.Println(err)
    return
  }
  stdin.Write([]byte("ok"))
  stdin.Close()
  err = cmd.Wait()
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(out.String())
}

func main() {
  useStdinStdout()
  useGoStdinPipe()
  useAsyncStdinPipe()
}
