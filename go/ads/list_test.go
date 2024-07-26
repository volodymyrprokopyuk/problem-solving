package ads_test

import (
	"testing"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func TestList(t *testing.T) {
  var lst ads.List
  lst.FromSlice([]int{1})
  expLength := 1
  gotLength := lst.Length()
  if gotLength != expLength {
    t.Errorf("invalid length: expected %v, got %v", expLength, gotLength)
  }
  expBackward := []int{1}
  gotBackward := make([]int, lst.Length())
  for i, val := range lst.Backward() {
    gotBackward[i] = val
  }
  if !slcEqual(gotBackward, expBackward) {
    t.Errorf("invalid backward: expected %v, got %v", expBackward, gotBackward)
  }
  expPeek := 1
  gotPeek, err := lst.Peek()
  if err != nil {
    t.Error(err)
  }
  if gotPeek != expPeek {
    t.Errorf("invalid peek: expected %v, got %v", expPeek, gotPeek)
  }
  expPop := 1
  gotPop, err := lst.Pop()
  if err != nil {
    t.Error(err)
  }
  if gotPop != expPop {
    t.Errorf("invalid pop: expected %v, got %v", expPop, gotPop)
  }
  gotPop, err = lst.Pop()
  if err == nil {
    t.Errorf("pop error expected, got no error")
  }
  gotPeek, err = lst.Peek()
  if err == nil {
    t.Errorf("peek error expected, got no error")
  }
}
