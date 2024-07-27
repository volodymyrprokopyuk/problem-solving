package main

import (
	"fmt"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func main() {
  var stk ads.Stack
  stk.FromSlice([]int{1, 2, 3})
  val, _ := stk.Peek()
  fmt.Println(val)
  for stk.Length() > 0 {
    val, _ := stk.Pop()
    fmt.Println(val)
  }
  stk.Push(10)
  stk.Push(20)
  for stk.Length() > 0 {
    val, _ := stk.Pop()
    fmt.Println(val)
  }

  // var que ads.Queue
  // que.FromSlice([]int{1, 2, 3})
  // val, _ := que.Peek()
  // fmt.Println(val)
  // for que.Length() > 0 {
  //   val, _ := que.Deq()
  //   fmt.Println(val)
  // }
  // que.Enq(10)
  // que.Enq(20)
  // for que.Length() > 0 {
  //   val, _ := que.Deq()
  //   fmt.Println(val)
  // }
}
