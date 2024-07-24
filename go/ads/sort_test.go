package ads_test

import (
	"testing"

	"github.com/volodymyrprokopyuk/problem-solving/go/ads"
)

func slcEqual(a, b []int) bool {
  if len(a) != len(b) {
    return false
  }
  for i := 0; i < len(a); i++ {
    if a[i] != b[i] {
      return false
    }
  }
  return true
}

type tcase struct {
  name string
  arr []int
  exp []int
}

func cases() []tcase {
  return []tcase{
    {"empty array", []int{}, []int{}},
    {"singleton array", []int{1}, []int{1}},
    {"sorted array", []int{1, 2, 3}, []int{1, 2, 3}},
    {"unordered array", []int{3, 1, 2}, []int{1, 2, 3}},
    {
      "duplicate elements",
      []int{3, 1, 4, 9, 2, 8, 5, 6, 7, 9, 0},
      []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9},
    },
  }
}

func TestBubbleSort(t *testing.T) {
  for _, c := range cases() {
    ads.BubbleSort(c.arr)
    if !slcEqual(c.arr, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, c.arr)
    }
  }
}

func TestInsertSort(t *testing.T) {
  for _, c := range cases() {
    ads.InsertSort(c.arr)
    if !slcEqual(c.arr, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, c.arr)
    }
  }
}

func TestShellSort(t *testing.T) {
  for _, c := range cases() {
    ads.ShellSort(c.arr)
    if !slcEqual(c.arr, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, c.arr)
    }
  }
}

func TestSelectSort(t *testing.T) {
  for _, c := range cases() {
    ads.SelectSort(c.arr)
    if !slcEqual(c.arr, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, c.arr)
    }
  }
}

func TestQuickSort(t *testing.T) {
  for _, c := range cases() {
    ads.QuickSort(c.arr)
    if !slcEqual(c.arr, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, c.arr)
    }
  }
}

func TestMergeSort(t *testing.T) {
  for _, c := range cases() {
    got := ads.MergeSort(c.arr)
    if !slcEqual(got, c.exp) {
      t.Errorf("slice is not sorted: expected %v, got %v", c.exp, got)
    }
  }
}
