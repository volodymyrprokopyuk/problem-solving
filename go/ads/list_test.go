package ads_test

import (
	"testing"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func TestList(t *testing.T) {
  var lst ads.List
  lst.FromSlice([]int{1, 2})
  expLength := 2
  gotLength := lst.Length()
  if gotLength != expLength {
    t.Errorf("invalid length: expected %v, got %v", expLength, gotLength)
  }
  expBackward := []int{2, 1}
  gotBackward := make([]int, lst.Length())
  for i, val := range lst.Backward() {
    gotBackward[i] = val
  }
  if !slcEqual(gotBackward, expBackward) {
    t.Errorf("invalid backward: expected %v, got %v", expBackward, gotBackward)
  }
  for _, _ = range lst.Backward() {
    break; // range iterator must stop
  }
  expPeek := 2
  gotPeek, err := lst.Peek()
  if err != nil {
    t.Error(err)
  }
  if gotPeek != expPeek {
    t.Errorf("invalid peek: expected %v, got %v", expPeek, gotPeek)
  }
  expPop := 2
  gotPop, err := lst.Pop()
  if err != nil {
    t.Error(err)
  }
  if gotPop != expPop {
    t.Errorf("invalid pop: expected %v, got %v", expPop, gotPop)
  }
  _, _ = lst.Pop()
  gotPop, err = lst.Pop()
  if err == nil {
    t.Errorf("pop error expected, got no error")
  }
  gotPeek, err = lst.Peek()
  if err == nil {
    t.Errorf("peek error expected, got no error")
  }
}
