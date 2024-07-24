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
  slc []int
  exp []int
}

func cases() []tcase {
  return []tcase{
    {"empty slice", []int{}, []int{}},
    {"singleton slice", []int{1}, []int{1}},
    {"sorted slice", []int{1, 2, 3}, []int{1, 2, 3}},
    {"unsorted slice", []int{3, 1, 2}, []int{1, 2, 3}},
    {
      "duplicate elements",
      []int{3, 1, 4, 9, 2, 8, 5, 6, 7, 9, 0},
      []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9},
    },
  }
}

func TestBubbleSort(t *testing.T) {
  for _, c := range cases() {
    ads.BubbleSort(c.slc)
    if !slcEqual(c.slc, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, c.slc)
    }
  }
}

func TestInsertSort(t *testing.T) {
  for _, c := range cases() {
    ads.InsertSort(c.slc)
    if !slcEqual(c.slc, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, c.slc)
    }
  }
}

func TestShellSort(t *testing.T) {
  for _, c := range cases() {
    ads.ShellSort(c.slc)
    if !slcEqual(c.slc, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, c.slc)
    }
  }
}

func TestSelectSort(t *testing.T) {
  for _, c := range cases() {
    ads.SelectSort(c.slc)
    if !slcEqual(c.slc, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, c.slc)
    }
  }
}

func TestQuickSort(t *testing.T) {
  for _, c := range cases() {
    ads.QuickSort(c.slc)
    if !slcEqual(c.slc, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, c.slc)
    }
  }
}

func TestMergeSort(t *testing.T) {
  for _, c := range cases() {
    got := ads.MergeSort(c.slc)
    if !slcEqual(got, c.exp) {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, got)
    }
  }
}

func TestSearch(t *testing.T) {
  cases := []struct{
    name string
    slc []int
    val int
    exp int
  }{
    {"empty slice", []int{}, 9, -1},
    {"singleton slice found", []int{1}, 1, 0},
    {"singleton slice not found", []int{1}, 9, -1},
    {"duplicate elements found", []int{1, 2, 2, 3}, 2, 1},
    {"larger slice found first", []int{1, 2, 3, 4}, 1, 0},
    {"larger slice found middle", []int{1, 2, 3, 4}, 2, 1},
    {"larger slice found middle center", []int{1, 2, 3, 4, 5}, 3, 2},
    {"larger slice found last", []int{1, 2, 3, 4}, 4, 3},
    {"larger slice not found", []int{1, 2, 3, 4}, 9, -1},
  }
  for _, c := range cases {
    got := ads.Search(c.slc, c.val)
    if got != c.exp {
      t.Errorf("%v: expected %v, got %v", c.name, c.exp, got)
    }
  }
}
