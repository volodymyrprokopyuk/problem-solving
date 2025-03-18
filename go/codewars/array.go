package codewars

import (
	"strconv"
	"strings"
)

// You get an array of numbers, return the sum of all of the positives ones.
func SumPositives(nums []int) int {
  sum := 0
  for _, num := range nums {
    if num > 0 {
      sum += num
    }
  }
  return sum
}

// Complete the square sum function so that it squares each number passed into
// it and then sums the results together.
func SumSquares(nums []int) int {
  sum := 0
  for _, num := range nums {
    sqr := num * num
    sum += sqr
  }
  return sum
}

// Consider an array/list of sheep where some sheep may be missing from their
// place. We need a function that counts the number of sheep present in the
// array (true means present).
func CountSheeps(sheeps []bool) int {
  cnt := 0
  for _, shp := range sheeps {
    if shp {
      cnt++
    }
  }
  return cnt
}

// Given a random non-negative number, you have to return the digits of this
// number within an array in reverse order.
func ReverseDigits(num int) []int {
  dgts := make([]int, 0, 20)
  for {
    rem := num % 10
    dgts = append(dgts, rem)
    num /= 10
    if num == 0 {
      break
    }
  }
  return dgts
}

// Given an array of integers, return a new array with each value doubled.
func DoubleArray(nums []int) []int {
  dbls := make([]int, len(nums))
  for i, num := range nums {
    dbls[i] = num * 2
  }
  return dbls
}

// Given a set of numbers, return the additive inverse of each. Each positive
// becomes negatives, and the negatives become positives.
func AdditiveInverse(nums []int) []int {
  invs := make([]int, len(nums))
  for i, num := range nums {
    invs[i] = 0 - num
  }
  return invs
}

// Given a string of digits, you should replace any digit below 5 with '0' and
// any digit 5 and above with '1'. Return the resulting string.
func FakeBin(dgts string) string {
  var bins strings.Builder
  for dgt := range strings.SplitSeq(dgts, "") {
    d, _ := strconv.Atoi(dgt)
    b := 0
    if d >= 5 {
      b = 1
    }
    bins.WriteString(strconv.Itoa(b))
  }
  return bins.String()
}
