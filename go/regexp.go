package main

import "fmt"
// import "strings"
import "regexp"

func useRegexp() {
  s := "a: 1, b: 2"
  r, err := regexp.Compile(`(?P<key>\w+): +(?P<val>\d+)`)
  if err != nil { fmt.Println(err); return }
  // match
  fmt.Println(r.MatchString(s)) // true
  // find
  fmt.Println(r.FindString(s)) // a: 1
  for _, m := range r.FindAllString(s, -1) {
    fmt.Println(m) // a: 1, b: 2
  }
  for _, gs := range r.FindAllStringSubmatch(s, -1) {
    fmt.Println(gs[1], "=>", gs[2]) // a => 1, b => 2
  }
  // replace
  fmt.Println(r.ReplaceAllString(s, "${val}: $key")) // 1: a, 2: b
  // split
  r, err = regexp.Compile(`, +`)
  if err != nil { fmt.Println(err); return }
  fmt.Println(r.Split(s, -1)) // a: 1, b: 2
}

func main() {
  useRegexp()
}
