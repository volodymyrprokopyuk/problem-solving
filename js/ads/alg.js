import { Queue, Heap, GNode } from "./dstr.js"

function lt(a, b) { return a < b }
function gt(a, b) { return a > b }

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n^2), memory O(1), in-place, stable
export function bubbleSort(arr, cmp = lt) {
  for (let i = arr.length - 1; i > 0; --i) {
    let swapped = false
    for (let j = 0; j < i; ++j) {
      if (cmp(arr[j + 1], arr[j])) {
        swap(arr, j, j + 1)
        swapped = true
      }
    }
    if (!swapped) { break }
  }
}

// O(n^2), memory O(1), in-place, not statlbe
export function selectionSort(arr, cmp = lt) {
  for (let i = 0; i < arr.length - 1; ++i) {
    let k = i
    for (let j = k + 1; j < arr.length; ++j) {
      if (cmp(arr[j], arr[k])) { k = j }
    }
    if (k !== i) { swap(arr, i, k) }
  }
}

// O(n^2), memory O(1), in-place, stable
export function insertionSort(arr, cmp = lt) {
  for (let i = 1; i < arr.length; ++i) {
    const el = arr[i]
    let j = i
    while (j > 0 && cmp(el, arr[j - 1])) {
      arr[j] = arr[j - 1]
      --j
    }
    if (j !== i) { arr[j] = el }
  }
}

// O(n*log(n)), memory O(n), copy, stable
export function mergeSort(arr, cmp = lt) {
  function merge(left, right) {
    const res = []
    let i = 0, j = 0
    while (i < left.length && j < right.length) {
      if (cmp(left[i], right[j])) { res.push(left[i++]) }
      else { res.push(right[j++]) }
    }
    while (i < left.length) { res.push(left[i++]) }
    while (j < right.length) { res.push(right[j++]) }
    return res
  }
  if (arr.length <= 1) { return arr }
  const m = Math.floor(arr.length / 2)
  const left = mergeSort(arr.slice(0, m), cmp)
  const right = mergeSort(arr.slice(m), cmp)
  return merge(left, right)
}

// O(n*log(n)), memory O(1), in-place, not stable
export function quickSort(arr, cmp = lt) {
  function sort(a, b) {
    if (a < b) {
      const p = arr[a + Math.floor((b - a) / 2)]
      let l = a, r = b
      while (l <= r) {
        while (cmp(arr[l], p)) { ++l }
        while (cmp(p, arr[r])) { --r }
        if (l <= r) { swap(arr, l, r); ++l; --r }
      }
      sort(a, r); sort(l, b)
    }
  }
  sort(0, arr.length - 1)
}

// O(n*log(n)), memory O(n), copy, not stable
export function heapSort(arr, cmp = lt) {
  const hp = Heap.from(arr, cmp)
  return Array.from(hp)
}

const arrs = [
  [], [0], [1, 2],
  [3, 2, 1],
  [9, 0, 2, 4, 6, 3, 8, 9, 7, 1, 5, 0]
]

// arrs.forEach(arr => { insertionSort(arr); console.log(arr) })
// arrs.forEach(arr => { console.log(heapSort(arr, gt)) })

// O(log(n))
export function binarySearch(arr, data, cmp = lt) {
  let a = 0, b = arr.length - 1
  while (a <= b) {
    const m = a + Math.floor((b - a) / 2)
    if (cmp(data, arr[m])) { b = m - 1 }
    else if (cmp(arr[m], data)) { a = m + 1 }
    else { return m }
  }
}

// const arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
// const els = [0, 1, 4, 7, 8, 9, 10]
// els.forEach(el => console.log(binarySearch(arr, el)))

// O(v + e)
export function depthFirstSearch(nd, fn) {
  const visited = new Map()
  function search(nd) {
    visited.set(nd.name)
    fn(nd)
    for (const adj of nd.adjs) {
      if (!visited.has(adj.name)) { search(adj) }
    }
  }
  search(nd)
}

// O(v + e)
export function breadthFirstSearch(nd, fn) {
  const visited = new Map()
  const qu = new Queue()
  visited.set(nd.name)
  qu.enqueue(nd)
  while (qu.length > 0) {
    nd = qu.dequeue()
    fn(nd)
    for (const adj of nd.adjs) {
      if (!visited.has(adj.name)) {
        visited.set(adj.name)
        qu.enqueue(adj)
      }
    }
  }
}

// const n0 = new GNode(0)
// const n1 = new GNode(1)
// const n2 = new GNode(2)
// const n3 = new GNode(3)
// const n4 = new GNode(4)
// const n5 = new GNode(5)
// n0.adjs.push(n1, n4, n5)
// n1.adjs.push(n3, n4)
// n2.adjs.push(n1)
// n3.adjs.push(n2, n4)
// depthFirstSearch(n0, nd => console.log(nd.name))
// breadthFirstSearch(n0, nd => console.log(nd.name))

export function* fibonacci(n) {
  let a = -1, b = 1
  while (n-- > 0) {
    const c = a + b
    a = b, b = c
    yield c
  }
}

// for (const el of fibonacci(10)) { console.log(el) }

export function powerSet(st) {
  const pSt = [[]]
  for (const el of st) {
    for (let i = 0, len = pSt.length; i < len; ++i) {
      pSt.push(pSt[i].concat(el))
    }
  }
  return pSt
}

// [[], [1], [1, 2], [1, 2, 3]].forEach(st => console.log(powerSet(st)))
