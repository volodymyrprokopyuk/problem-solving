import { Heap } from "./heap.js"
import { HTable, HSet } from "./htable.js"
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

// O(n^2*m) returns the top k most similar sited based on an access log
export function topSimilarSites(k, accessLog) {
  function similarity(site1, site2) {
    const siteVis1 = visitors.get(site1), siteVis2 = visitors.get(site2)
    return siteVis1.isect(siteVis2).length / siteVis1.union(siteVis2).length
  }
  const visitors = new HTable()
  for (const [site, user] of accessLog) {
    let siteVis = visitors.get(site)
    if (siteVis) { siteVis.set(user) }
    else {
      siteVis = new HSet(); siteVis.set(user); visitors.set(site, siteVis)
    }
  }
  const maxHeap = new Heap((a, b) => a.sim < b.sim)
  const sitePairs = new HSet()
  for (const [site1, _] of visitors) {
    for (const [site2, _] of visitors) {
      if (site1 !== site2 && !sitePairs.get(site2 + site1)) {
        sitePairs.set(site1 + site2)
        maxHeap.push({ sim: similarity(site1, site2), pair: [site1, site2] })
      }
    }
  }
  const topSites = []
  while (maxHeap.length > 0 && k-- > 0) { topSites.push(maxHeap.pop()) }
  return topSites
}

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
    if (!nd.left && !nd.right) { htb.set(nd.value, code); return }
    encode(nd.left, code + "0"); encode(nd.right, code + "1")
  }
  const minHeap = Heap.from(
    [...freq].map(([sym, freq]) => new TNode(freq, sym)),
    (a, b) => a.key > b.key
  )
  while (minHeap.length > 1) {
    const left = minHeap.pop(), right = minHeap.pop()
    const nd = new TNode(left.key + right.key, "")
    nd.left = left; nd.right = right
    minHeap.push(nd)
  }
  const htb = new HTable()
  encode(minHeap.peek())
  return htb
}
