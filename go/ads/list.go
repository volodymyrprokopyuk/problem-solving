// package ads
package main

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
    for n != nil && yield(i, n.value) {
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

type DList struct {
  head, tail *Node
  length int
}

func (l *DList) Length() int {
  return l.length
}

func (l *DList) FromSlice(slc []int) {
  for _, val := range slc {
    // l.PushHead(val)
    l.PushTail(val)
  }
}

func (l *DList) Backward() func(yield func(i, val int) bool) {
  i, nd := 0, l.head
  return func(yield func(i, val int) bool) {
    for nd != nil && yield(i, nd.value) {
      nd = nd.next
      i++
    }
  }
}

func (l *DList) Forward() func(yield func(i, val int) bool) {
  i, nd := 0, l.tail
  return func(yield func(i, val int) bool) {
    for nd != nil && yield(i, nd.value) {
      nd = nd.prev
      i++
    }
  }
}

// O(1)
func (l *DList) PushHead(val int) {
  nd := &Node{value: val}
  if l.head == nil {
    l.head, l.tail = nd, nd
    return
  }
  nd.next = l.head
  l.head.prev = nd
  l.head = nd
  l.length++
}

// O(1)
func (l *DList) PushTail(val int) {
  nd := &Node{value: val}
  if l.tail == nil {
    l.head, l.tail = nd, nd
    return
  }
  l.tail.next = nd
  nd.prev = l.tail
  l.tail = nd
  l.length++
}

func main() {
  var lst DList
  lst.FromSlice([]int{1, 2, 3, 4})
  for _, val := range lst.Backward() {
    fmt.Printf("%v ", val)
  }
  fmt.Println()
  for _, val := range lst.Forward() {
    fmt.Printf("%v ", val)
  }
  fmt.Println()
}
