package ads

import "fmt"

type Heap struct {
  slice []int
}

func (h *Heap) FromSlice(slc []int) {
  for _, val := range slc {
    h.Push(val)
  }
}

// O(log(n))
func (h *Heap) Push(val int) {
  heapUp := func(chd int) {
    par := (chd - 1) / 2
    for h.slice[chd] < h.slice[par] {
      h.slice[chd], h.slice[par] = h.slice[par], h.slice[chd]
      chd = par
      par = (chd - 1) / 2
    }
  }
  h.slice = append(h.slice, val)
  heapUp(len(h.slice) - 1)
}

// O(1)
func (h *Heap) Peek() (int, error) {
  if len(h.slice) == 0 {
    return 0, fmt.Errorf("peek from empty heap")
  }
  return h.slice[0], nil
}

// O(log(n))
func (h *Heap) Pop() (int, error) {
  child := func(par int) int {
    lft, rht := 2 * par + 1, 2 * par + 2
    switch {
    case rht < len(h.slice):
      if h.slice[lft] < h.slice[rht] {
        return lft
      } else {
        return rht
      }
    case lft < len(h.slice):
      return lft
    default:
      return -1
    }
  }
  heapDown := func() {
    par := 0
    chd := child(par)
    for chd != -1 && h.slice[chd] < h.slice[par] {
      h.slice[chd], h.slice[par] = h.slice[par], h.slice[chd]
      par = chd
      chd = child(par)
    }
  }
  if len(h.slice) == 0 {
    return 0, fmt.Errorf("pop from empty heap")
  }
  val := h.slice[0]
  h.slice[0] = h.slice[len(h.slice) - 1]
  heapDown()
  return val, nil
}
