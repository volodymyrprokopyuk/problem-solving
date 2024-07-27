package ads

type Queue struct {
  lst DList
}

func (q *Queue) Length() int {
  return q.lst.Length()
}

func (q *Queue) FromSlice(slc []int) {
  for _, val := range slc {
    q.Enq(val)
  }
}

// O(1)
func (q *Queue) Enq(val int) {
  q.lst.PushTail(val)
}

// O(1)
func (q *Queue) Peek() (int, error) {
  return q.lst.PeekHead()
}

// O(1)
func (q *Queue) Deq() (int, error) {
  return q.lst.PopHead()
}
