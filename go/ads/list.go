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
  nd := l.head
  l.head = l.head.next
  l.length--
  return nd.value, nil
}

func (l *List) Reverse() {
  var prev, next *Node
  nd := l.head
  for nd != nil {
    next = nd.next
    nd.next = prev
    prev = nd
    nd = next
  }
  l.head = prev
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
    l.PushHead(val)
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
  l.length++
  if l.head == nil {
    l.head, l.tail = nd, nd
    return
  }
  nd.next = l.head
  l.head.prev = nd
  l.head = nd
}

// O(1)
func (l *DList) PushTail(val int) {
  nd := &Node{value: val}
  l.length++
  if l.tail == nil {
    l.head, l.tail = nd, nd
    return
  }
  nd.prev = l.tail
  l.tail.next = nd
  l.tail = nd
}

// O(1)
func (l *DList) PeekHead() (int, error) {
  if l.head == nil {
    return 0, fmt.Errorf("peek head from empty dlist")
  }
  return l.head.value, nil
}

// O(1)
func (l *DList) PeekTail() (int, error) {
  if l.tail == nil {
    return 0, fmt.Errorf("peek tail from empty dlist")
  }
  return l.tail.value, nil
}

// O(1)
func (l *DList) PopHead() (int, error) {
  if l.head == nil {
    return 0, fmt.Errorf("pop head from empty dlist")
  }
  nd := l.head
  l.length--
  if l.head.next == nil {
    l.head, l.tail = nil, nil
    return nd.value, nil
  }
  l.head = l.head.next
  l.head.prev = nil
  return nd.value, nil
}

// O(1)
func (l *DList) PopTail() (int, error) {
  if l.tail == nil {
    return 0, fmt.Errorf("pop tail from empty dlist")
  }
  nd := l.tail
  l.length--
  if l.tail.prev == nil {
    l.head, l.tail = nil, nil
    return nd.value, nil
  }
  l.tail = l.tail.prev
  l.tail.next = nil
  return nd.value, nil
}

// O(1)
func (l *DList) Insert(val int, nd *Node) {
  newnd := &Node{value: val}
  l.length++
  newnd.next = nd.next
  newnd.prev = nd
  if nd == l.tail {
    l.tail = newnd
  } else {
    nd.next.prev = newnd
  }
  nd.next = newnd
}

// O(1)
func (l *DList) Delete(nd *Node) {
  l.length--
  if nd == l.head {
    l.head = l.head.next
    l.head.prev = nil
    return
  }
  if nd == l.tail {
    l.tail = l.tail.prev
    l.tail.next = nil
    return
  }
  nd.prev.next = nd.next
  nd.next.prev = nd.prev
}
