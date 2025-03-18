package codewars_test

import (
	"slices"
	"testing"

	"github.com/volodymyrprokopyuk/go/codewars"
)

func TestSumPositives(t *testing.T) {
  nums := []int{1, -4, 7, 12}
  exp := 20
  sum := codewars.SumPositives(nums)
  if sum != exp {
    t.Errorf("SumPositive: expected %d, got %d", exp, sum)
  }
}

func TestSumSquares(t *testing.T) {
  nums := []int{1, 2, 2}
  exp := 9
  sum := codewars.SumSquares(nums)
  if sum != exp {
    t.Errorf("SumSquares: expected %d, got %d", exp, sum)
  }
}

func TestCountSheeps(t *testing.T) {
  sheeps := []bool{
    true, true, true, false, true, true, true, true , true, false, true, false,
    true, false, false, true , true, true, true, true , false, false, true, true,
  }
  exp := 17
  cnt := codewars.CountSheeps(sheeps)
  if cnt != exp {
    t.Errorf("CountSheeps: expected %d, got %d", exp, cnt)
  }
}

func TestReverseDigits(t *testing.T) {
  num := 35231
  exp := []int{1, 3, 2, 5, 3}
  dgts := codewars.ReverseDigits(num)
  if !slices.Equal(dgts, exp) {
    t.Errorf("ReverseDigits: expected %v, got %v", exp, dgts)
  }
}

func TestDouleArray(t *testing.T) {
  nums := []int{1, 2, 3}
  exp := []int{2, 4, 6}
  dbls := codewars.DoubleArray(nums)
  if !slices.Equal(dbls, exp) {
    t.Errorf("DoubleArray: expected %v, got %v", exp, dbls)
  }
}

func TestAdditiveInverse(t *testing.T) {
  nums := []int{1, -2, 3, -4, 5}
  exp := []int{-1, 2, -3, 4, -5}
  invs := codewars.AdditiveInverse(nums)
  if !slices.Equal(invs, exp) {
    t.Errorf("AdditiveInverse: expected %v, got %v", exp, invs)
  }
}

func TestFakeBin(t *testing.T) {
  dgts := "16385902"
  exp := "01011100"
  bins := codewars.FakeBin(dgts)
  if bins != exp {
    t.Errorf("FakeBin: expected %v, got %v", exp, bins)
  }
}
