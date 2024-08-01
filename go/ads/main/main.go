package main

import (
	"fmt"

	// "github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func ContainsFunc[T comparable](slc []T, pred func(val T) bool) bool {
  for _, val := range slc {
    if pred(val) {
      return true
    }
  }
  return false
}

func main() {
  slc := []int{1, 21, 3, 41}
  fmt.Println(ContainsFunc(slc, func(val int) bool { return val % 2 == 0 }))


  // set := ads.NewSet[int](1, 2, 3, 1, 4)
  // set.Delete(4)
  // fmt.Println(set.Length())
  // fmt.Println(set.Get(2), set.Get(9))
  // fmt.Println(set.Union(ads.NewSet(10, 20)))
  // fmt.Println(set.Intersect(ads.NewSet(2, 3, 4)))
  // fmt.Println(set.Diff(ads.NewSet(2, 3, 4)))

  // heap.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
  // bst.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
}
