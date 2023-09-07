import { Heap } from "./heap.js"

// O(n*log(n)) incrementally computes a median of a stream of numbers
export function runningMedian(arr) {
  function median() {
    return minHeap.length > maxHeap.length ? minHeap.peek() :
      minHeap.length < maxHeap.length ? maxHeap.peek() :
      (minHeap.peek() + maxHeap.peek()) / 2
  }
  function add(el) {
    minHeap.length === 0 && maxHeap.length === 0 ? minHeap.push(el) :
      el > median() ? minHeap.push(el) : maxHeap.push(el)
  }
  function rebalance() {
    if (minHeap.length + 1 < maxHeap.length) {
      minHeap.push(maxHeap.pop())
    } else if (maxHeap.length + 1 < minHeap.length) {
      maxHeap.push(minHeap.pop())
    }
  }
  const minHeap = new Heap((a, b) => a > b)
  const maxHeap = new Heap((a, b) => a < b)
  return arr.map(el => { add(el); rebalance(); return median() })
}

// TODO Daily, p. 109, Find most similar web sites. HTable

// O(n*log(n)) generates the n first regular numbers
export function regularNumbers(n) {
  const minHeap = Heap.from([1], ((a, b) => a > b)), nums = []
  let last = 0
  while (nums.length < n) {
    const num = minHeap.pop()
    if (num > last) {
      nums.push(num); last = num
      minHeap.push(num * 2).push(num * 3).push(num * 5)
    }
  }
  return nums
}

// TODO Daily, p. 113, Build a Huffman tree. HTable
