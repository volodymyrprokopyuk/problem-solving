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

const arr = [2, 1, 5, 7, 2, 0, 5]
console.log(runningMedian(arr))
