package ads

import "fmt"

// O(n^2), in-place, stable, moving target
func BubbleSort[T ~int | ~string](slc []T) {
  for i := len(slc) - 1; i > 0; i-- {
    swap := false
    for j := 0; j < i; j++ {
      if slc[j] > slc[j + 1] {
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
    for j > 0 && slc[j - 1] > e {
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
      for j := i; j - gap + 1 > 0 && slc[j - gap] > slc[j]; j-- {
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

func main() {
  slc := []int{3, 1, 4, 9, 2, 8, 5, 6, 7, 9, 0}
  // BubbleSort(slc)
  // InsertSort(slc)
  ShellSort(slc)
  // SelectSort(slc)
  fmt.Println(slc)
}
