package ads

import "fmt"

type Node struct {
  value int
  next, prev *Node
}

type List struct {
  head *Node
  length int
}

func (l *List) Length() int {
  return l.length
}

func (l *List) FromSlice(slc []int) {
  for _, val := range slc {
    l.Push(val)
  }
}

func (l *List) Backward() func(yield func(i, val int) bool) {
  i, n := 0, l.head
  return func(yield func(i, val int) bool) {
    for n != nil {
      yield(i, n.value)
      n = n.next
      i++
    }
  }
}

// O(1)
func (l *List) Push(val int) {
  nd := &Node{value: val}
  nd.next = l.head
  l.head = nd
  l.length++
}

// O(1)
func (l *List) Peek() (int, error) {
  if l.head == nil {
    return 0, fmt.Errorf("peek from empty list")
  }
  return l.head.value, nil
}

// O(1)
func (l *List) Pop() (int, error) {
  if l.head == nil {
    return 0, fmt.Errorf("pop from empty list")
  }
  val := l.head.value
  l.head = l.head.next
  l.length--
  return val, nil
}
