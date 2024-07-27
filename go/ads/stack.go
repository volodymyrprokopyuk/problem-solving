package ads

type Stack struct {
  lst List
}

func (s *Stack) Length() int {
  return s.lst.Length()
}

func (s *Stack) FromSlice(slc []int) {
  for _, val := range slc {
    s.Push(val)
  }
}

// O(1)
func (s *Stack) Push(val int) {
  s.lst.Push(val)
}

// O(1)
func (s *Stack) Peek() (int, error) {
  return s.lst.Peek()
}

// O(1)
func (s *Stack) Pop() (int, error) {
  return s.lst.Pop()
}
