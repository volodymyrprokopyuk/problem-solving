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

func MergeMaps[K comparable, V any, M map[K]V](maps ...M) map[K]V {
  res := make(map[K]V)
  for _, m := range maps {
    for k, v := range m {
      res[k] = v
    }
  }
  return res
}

func main() {
  // slc := []int{1, 21, 3, 41}
  // fmt.Println(ContainsFunc(slc, func(val int) bool { return val % 2 == 0 }))

  // map1 := map[string]int{"a": 1, "b": 2}
  // map2 := map[string]int{"a": 10, "c": 3}
  // fmt.Println(MergeMaps(map1,map2))

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
