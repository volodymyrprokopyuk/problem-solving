package main

import (
	"fmt"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func main() {
  var heap ads.Heap
  heap.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
  val, _ := heap.Pop()
  fmt.Println(val)
  val, _ = heap.Pop()
  fmt.Println(val)
  val, _ = heap.Pop()
  fmt.Println(val)
  val, _ = heap.Peek()
  fmt.Println(val)

  // bst.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
}
