package main

import "fmt"
import "strings"
import "io"

func produce(w io.Writer) error {
  s, n := "Go is powerful", 4
  i, j := 0, n
  for ; j < len(s); i, j = i + n, j + n {
    _, err := w.Write([]byte(s[i:j]))
    if err != nil {
      return err
    }
  }
  if i < len(s) {
    _, err := w.Write([]byte(s[i:]))
    if err != nil {
      return err
    }
  }
  if cl, ok := w.(io.Closer); ok {
    cl.Close() // EOF
  }
  return nil
}

func consume(r io.Reader) error {
  buf := make([]byte, 4)
  for {
    n, err := r.Read(buf)
    if n > 0 {
      fmt.Println(string(buf[:n]))
    }
    if err == io.EOF {
      break
    }
    if err != nil {
      return err
    }
  }
  return nil
}

func useProduce() {
  var b strings.Builder
  err := produce(&b)
  if err != nil {
    fmt.Println(err); return
  }
  fmt.Println(b.String())
}

func useConsume() {
  r := strings.NewReader("Go is powerful")
  err := consume(r)
  if err != nil {
    fmt.Println(err); return
  }
}

func usePipe() {
  // pipe = is unbuffered sync: Write will block until data is Read from a pipe
  pr, pw := io.Pipe()
  go produce(pw)
  consume(pr)
}

func main() {
  useProduce()
  useConsume()
  usePipe()
}
