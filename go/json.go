package main

import (
  "encoding/json"
  "fmt"
  "io"
  "strings"
)

type Bayan struct { // fields must be exported
  Model string `json:"model"`
  Year int `json:"year"`
  Price float64 `json:"price"`
  Omit bool `json:"-"`
}

func (b Bayan) MarshalJSON() ([]byte, error) { // value receiver
  m := map[string]any{"aModel": b.Model, "aYear": b.Year, "aPrice": b.Price}
  return json.Marshal(m)
}

func (b *Bayan) UnmarshalJSON(j []byte) error { // pointer receiver
  var m map[string]any
  err := json.Unmarshal(j, &m)
  if err != nil {
    return err
  }
  if model, ok := m["aModel"].(string); ok {
    b.Model = model
  }
  if year, ok := m["aYear"].(float64); ok {
    b.Year = int(year)
  }
  if price, ok := m["aPrice"].(float64); ok {
    b.Price = price
  }
  return nil
}

func marshalStruct() ([]byte, error) {
  ba := Bayan{"Nextra", 2023, 16e3, true}
  j, err := json.Marshal(ba) // returns []byte
  if err != nil {
    return []byte{}, err
  }
  return j, nil
}

func unmarshalStruct(j []byte) {
  var ba Bayan
  err := json.Unmarshal(j, &ba) // accepts []byte
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(ba)
}

func encodeJSON() (string, error) {
  var (b = true; i = 1; f = 1.2; c = 'a'; s = "ab")
  var ia = []int{1, 2, 3}
  var sa = []string{"ab", "cd"}
  var m = map[string]float64{"a": 1.2, "b": 3.4}
  var ba = Bayan{"Nextra", 2023, 16e3, true}
  var w strings.Builder
  enc := json.NewEncoder(&w)
  for _, v := range []any{b, i, f, c, s, ia, sa, m, ba} {
    err := enc.Encode(v)
    if err != nil {
      return "", err
    }
  }
  return w.String(), nil
}

func decodeJSON(j string) {
  var (b2 bool; i2 int; f2 float64; c2 rune; s2 string)
  var (ia2 []int; sa2 []string; m2 map[string]float64; ba2 Bayan)
  vals := []any{&b2, &i2, &f2, &c2, &s2, &ia2, &sa2, &m2, &ba2}
  r := strings.NewReader(j);
  dec := json.NewDecoder(r)
  for i := 0; i < len(vals); i++ {
    err := dec.Decode(vals[i])
    if err == io.EOF {
      break
    }
    if err != nil {
      fmt.Println(err)
      return
    }
  }
  fmt.Printf(
    "%T %v\n%T %v\n%T %v\n%T %v\n%T %v\n%T %v\n%T %v\n%T %v\n%T %v\n",
    b2, b2, i2, i2, f2, f2, c2, c2, s2, s2, ia2, ia2, sa2, sa2, m2, m2, ba2, ba2,
  )
}

func main() {
  j, err := marshalStruct()
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(string(j))
  unmarshalStruct(j)

  j2, err := encodeJSON()
  if err != nil {
    fmt.Println(err)
    return
  }
  fmt.Println(j2)
  decodeJSON(j2)
}
