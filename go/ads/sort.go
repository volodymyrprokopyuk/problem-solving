// package main
package ads

import "fmt"

// O(n^2), in-place, stable, moving target
func BubbleSort[T ~int | ~string](slc []T) {
  for i := len(slc) - 1; i > 0; i-- {
    swap := false
    for j := 0; j < i; j++ {
      if slc[j + 1] < slc[j] {
        swap = true
        slc[j], slc[j + 1] = slc[j + 1], slc[j]
      }
    }
    if !swap {
      break
    }
  }
}

// O(n^2), in-place, stable, fixed target, O(n) on sorted array
func InsertSort[T ~int | ~string](slc []T) {
  for i := 1; i < len(slc); i++ {
    e := slc[i]
    j := i
    for j > 0 && e < slc[j - 1] {
      slc[j] = slc[j - 1]
      j--
    }
    if j != i {
      slc[j] = e
    }
  }
}

// O(n*log(n)), in-place, non-stable, generalization of InsertSort
func ShellSort[T ~int | ~string](slc []T) {
  for _, gap := range []int{31, 15, 7, 3, 1} {
    for i := gap; i < len(slc); i++ {
      for j := i; j - gap + 1 > 0 && slc[j] < slc[j - gap]; j-- {
        slc[j - gap], slc[j] = slc[j], slc[j - gap]
      }
    }
  }
}

// O(n^2), in-place, non-stable, fixed target
func SelectSort[T ~int | ~string](slc []T) {
  for i := 0; i < len(slc) - 1; i++ {
    m := i
    for j := i + 1; j < len(slc); j++ {
      if slc[j] < slc[m] {
        m = j
      }
    }
    if m != i {
      slc[i], slc[m] = slc[m], slc[i]
    }
  }
}

func merge(a, b []int) []int {
  r := make([]int, 0, len(a) + len(b))
  i, j := 0, 0
  for i < len(a) && j < len(b) {
    if b[j] < a[i] {
      r = append(r, b[j])
      j++
    } else {
      r = append(r, a[i])
      i++
    }
  }
  for i < len(a) {
    r = append(r, a[i])
    i++
  }
  for j < len(b) {
    r = append(r, b[j])
    j++
  }
  return r
}

// O(n*log(n)), copy, stable, external sorting in files
func MergeSort(slc []int) []int {
  if len(slc) < 2 {
    return slc
  }
  m := len(slc) / 2
  return merge(MergeSort(slc[:m]), MergeSort(slc[m:]))
}

// O(n*log(n)), in-place, non-stable
func QuickSort(slc []int) {
  partition := func(a, b int) int {
    p := slc[b - 1] // pivot is the last element
    i := a
    for j := a; j < b - 1; j++ {
      if slc[j] < p {
        slc[i], slc[j] = slc[j], slc[i]
        i++
      }
    }
    slc[i], slc[b - 1] = slc[b - 1], slc[i]
    return i
  }
  var sort func(a, b int) // recursive function expression
  sort = func(a, b int) {
    if b - a < 2 {
      return
    }
    p := partition(a, b)
    sort(a, p); sort(p + 1, b)
  }
  sort(0, len(slc))
}

func main() {
  slc := []int{3, 1, 4, 9, 2, 8, 5, 6, 7, 9, 0}
  // BubbleSort(slc)
  // InsertSort(slc)
  // ShellSort(slc)
  // SelectSort(slc)
  QuickSort(slc)
  fmt.Println(slc)
  // fmt.Println(MergeSort(slc))
}
