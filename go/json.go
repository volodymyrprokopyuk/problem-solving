package main

import (
  "fmt"
  "bytes"
  "encoding/json"
  "os"
)

type Bayan struct {
  Model string `json:"model"`
  Year int `json:"year"`
  Price float64 `json:"price"`
  Omit string `json:"omit,omitempty"`
}

// custom marshaller
func (b Bayan) MarshalJSON() ([]byte, error) { // value receiver
  m := map[string]any{"aModel": b.Model, "aYear": b.Year, "aPrice": b.Price}
  return json.Marshal(m)
}

// custom unmarshaller
func (b *Bayan) UnmarshalJSON(j []byte) error { // pointer receiver
  var m map[string]any
  err := json.Unmarshal(j, &m)
  if err != nil {
    return err
  }
  // type assertions
  if model, assert := m["aModel"].(string); assert {
    b.Model = model
  }
  // JSON number is always a float64
  if year, assert := m["aYear"].(float64); assert {
    b.Year = int(year) // explicit conversion
  }
  if price, assert := m["aPrice"].(float64); assert {
    b.Price = price
  }
  return nil
}

func exitOnError(err error) {
  if err != nil {
    fmt.Println(err)
    os.Exit(1)
  }
}

func main() {
  b := Bayan{"Nextra", 2024, 16e3, ""}

  // marshal a bayan to a JSON byte array
  j, err := json.Marshal(b)
  exitOnError(err)
  fmt.Printf("%s\n", j)

  // unmarshal a JSON byte array to a bayan
  var b2 Bayan
  err = json.Unmarshal(j, &b2)
  exitOnError(err)
  fmt.Printf("%+v\n", b2)

  // encode a bayan to a JSON stream
  var buf bytes.Buffer
  enc := json.NewEncoder(&buf)
  err = enc.Encode(b)
  exitOnError(err)
  fmt.Print(buf.String())

  // decode a JSON stream to a bayan
  r := bytes.NewReader(j)
  dec := json.NewDecoder(r)
  err = dec.Decode(&b2)
  exitOnError(err)
  fmt.Printf("%+v\n", b2)
}
