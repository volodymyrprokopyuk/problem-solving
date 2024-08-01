package ads

type Set[K comparable] map[K]struct{}

func NewSet[K comparable](keys ...K) *Set[K] {
  var set Set[K] = make(map[K]struct{})
  for _, key := range keys {
    set.Set(key)
  }
  return &set
}

func (s *Set[K]) Length() int {
  return len(*s)
}

func (s *Set[K]) Set(keys ...K) {
  for _, key := range keys {
    (*s)[key] = struct{}{}
  }
}

func (s *Set[K]) Delete(keys ...K) {
  for _, key := range keys {
    delete(*s, key)
  }
}

func (s *Set[K]) Get(key K) bool {
  _, exist := (*s)[key]
  return exist
}

func (s *Set[K]) Union(oth *Set[K]) *Set[K] {
  res := NewSet[K]()
  for key := range *s {
    res.Set(key)
  }
  for key := range *oth {
    res.Set(key)
  }
  return res
}

func (s *Set[K]) Intersect(oth *Set[K]) *Set[K] {
  res := NewSet[K]()
  for key := range *s {
    if oth.Get(key) {
      res.Set(key)
    }
  }
  return res
}

func (s *Set[K]) Diff(oth *Set[K]) *Set[K] {
  res := NewSet[K]()
  for key := range *s {
    if !oth.Get(key) {
      res.Set(key)
    }
  }
  return res
}
