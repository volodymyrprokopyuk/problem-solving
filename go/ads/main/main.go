package main

import (
	"fmt"
	"io"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func printAny[T any](w io.Writer, val T) {
  fmt.Fprintf(w, "%v", val)
}

func printAll[T any](val ...T) {
  for _, v := range val {
    fmt.Printf("%v ", v)
  }
}

func length[T any](slc []T) int {
  return len(slc)
}

func main() {
  // printAny(os.Stdout, 1)
  // printAny(os.Stdout, "a")
  // printAll(1, 2, 3)
  // fmt.Println(length([]int{1, 2}), length([]bool{true, false}))

  // trie := ads.NewTrie()
  // trie.Set("Vlad")
  // fmt.Println(trie.Get("Vlad"), trie.Get("vlad"), trie.Get("x"), trie.Get(""))

  // heap.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
  // bst.FromSlice([]int{8, 1, 3, 2, 6, 0, 5, 4, 7, 9})
}
