import { Heap } from "./heap.js"
import { HTable } from "./htable.js"
import { TNode } from "./tree.js"

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

// O(n*log(n)) returns encoding for characters with a given occurence frequency
export function huffmanEncode(freq) {
  function encode(nd, code = "") {
    if (!nd.left && !nd.right) { htb.set(nd.data, code); return }
    encode(nd.left, code + "0"); encode(nd.right, code + "1")
  }
  const minHeap = Heap.from(
    [...freq].map(([sym, freq]) => new TNode(freq, sym)),
    (a, b) => a.key > b.key
  )
  while (minHeap.length > 1) {
    const left = minHeap.pop(), right = minHeap.pop()
    console.log(left, right)
    const nd = new TNode(left.key + right.key, "")
    nd.left = left; nd.right = right
    minHeap.push(nd)
  }
  const htb = new HTable()
  encode(minHeap.peek())
  return htb
}
