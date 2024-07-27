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

type Deque struct {
  lst DList
}

func (d *Deque) Length() int {
  return d.lst.Length()
}

func (d *Deque) FromSlice(slc []int) {
  for _, val := range slc {
    d.EnqRear(val)
  }
}

func (d *Deque) EnqFront(val int) {
  d.lst.PushHead(val)
}

func (d *Deque) EnqRear(val int) {
  d.lst.PushTail(val)
}

func (d *Deque) PeekFront() (int, error) {
  return d.lst.PeekHead()
}

func (d *Deque) PeekRear() (int, error) {
  return d.lst.PeekTail()
}

func (d *Deque) DeqFront() (int, error) {
  return d.lst.PopHead()
}

func (d *Deque) DeqRear() (int, error) {
  return d.lst.PopTail()
}
