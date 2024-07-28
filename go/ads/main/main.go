package main

import (
	"fmt"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func main() {
  var bst ads.BSTree
  bst.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
  err := bst.Delete(8)
  if err != nil {
    fmt.Println(err)
  }
  for _, val := range bst.InOrder() {
    fmt.Printf("%v ", val)
  }
  fmt.Println()
}
