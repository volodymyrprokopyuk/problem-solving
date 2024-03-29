import { Stack, Queue, BSTree, Heap } from "./dstructure.js"

function error(msg) { throw new Error(`ERROR: ${msg}`) }

export function lt(a, b) { return a < b ? -1 : a > b ? 1 : 0 }
export function gt(a, b) { return a > b ? -1 : a < b ? 1 : 0 }

// ** Array

// O(n), memory O(1)
export function isUnique(arr) {
  const viewed = { }
  for (const el of arr) {
    if (viewed[el]) { return false }
    viewed[el] = true
  }
  return true
}

// O(n), memory O(n)
export function isUniqueCmp(arr, cmp = lt) {
  const srt = sortQuickCp(arr, lt)
  for (let i = 0; i < srt.length - 1; ++i) {
    if (cmp(srt[i], srt[i + 1]) === 0) { return false }
  }
  return true
}

// O(log n), memory O(1)
export function bsFind(arr, vl) {
  let l = 0
  let r = arr.length - 1
  while (l <= r) {
    const m = l + Math.floor((r - l) / 2)
    console.log(m, arr[m])
    if (vl === arr[m]) { return m }
    if (vl < arr[m]) { r = m -1 }
    else { l = m + 1 }
  }
  return -1
}

// O(log n), memory O(log n)
export function bsFindRec(arr, vl) {
  function find(l, r) {
    if (l > r) { return -1 }
    const m = l + Math.floor((r - l) / 2)
    if (vl === arr[m]) { return m }
    return vl < arr[m] ? find(l, m - 1) : find(m + 1, r)
  }
  return find(0, arr.length - 1)
}

// ** Stack

// O(n)
export function reverse(arr) {
  const st = Stack.from(arr)
  const res = []
  while (st.length > 0) { res.push(st.pop()) }
  return res
}

// const arr = [1, 2, 3, 4, 5]
// console.log(reverse([]), reverse(arr))

// ** Tree

// O(n)
export function bstInOrder(nd, fn) {
  if (nd !== null) {
    bstInOrder(nd.left, fn)
    // Node in the middle
    fn(nd)
    bstInOrder(nd.right, fn)
  }
}

// O(n)
export function bstPreOrder(nd, fn) {
  if (nd !== null) {
    // Node first
    fn(nd)
    bstPreOrder(nd.left, fn)
    bstPreOrder(nd.right, fn)
  }
}

// O(n)
export function bstPostOrder(nd, fn) {
  if (nd !== null) {
    bstPostOrder(nd.left, fn)
    bstPostOrder(nd.right, fn)
    // Node last
    fn(nd)
  }
}

// const tr = BSTree.from([3, 1, 4, 5, 2])
// let arr = []
// bstInOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)
// arr = []
// bstPreOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)
// arr = []
// bstPostOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)

// O(log n)
export function bstFind(nd, vl) {
  // Stop on terminal node
  if (nd === null) { return null }
  // Element is found
  if (vl === nd.data) { return nd }
  // Binary search on a BSTree
  return vl < nd.data ? bstFind(nd.left, vl) : bstFind(nd.right, vl)
}

// O(log n)
export function bstMin(nd) {
  if (nd === null) { error("bstMin on null") }
  // Minimum is the leftmost child node
  return nd.left === null ? nd : bstMin(nd.left)
}

// O(log n)
export function bstMax(nd) {
  if (nd === null) { error("bstMax on null") }
  // Maximum is the rightmost child node
  return nd.right === null ? nd : bstMax(nd.right)
}

// const tr = BSTree.from([8, 1, 4, 3, 2, 6, 7, 9, 0, 5])
// console.log(bstFind(tr.root, 1)?.data, bstFind(tr.root, -1)?.data)
// console.log(bstMin(tr.root)?.data, bstMax(tr.root)?.data)

// ** Graph

// O(v + e)
export function dfSearch(nd, fn) {
  const visited = { }
  function search(nd) {
    if (nd === null) { return }
    fn(nd)
    visited[nd.name] = true
    for (const adj of nd.adjs) {
      if (!visited[adj.name]) { search(adj) }
    }
  }
  search(nd)
}

// O(v + e)
export function bfSearch(nd, fn) {
  if (nd === null) { return }
  const visited = { }
  const qu = new Queue()
  qu.enq(nd)
  visited[nd.name] = true
  while (qu.length > 0) {
    nd = qu.deq()
    fn(nd)
    for (const adj of nd.adjs) {
      if (!visited[adj.name]) {
        qu.enq(adj)
        visited[adj.name] = true
      }
    }
  }
}

// ** Sorting

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n^2), memory O(1), in-place
export function sortBubbleIn(arr, cmp = (a, b) => a < b) {
  for (let i = arr.length - 1; i > 0; --i) {
    let swp = false
    for (let j = 0; j < i; ++j) {
      if (cmp(arr[j + 1], arr[j])) {
        swap(arr, j, j + 1)
        swp = true
      }
    }
    if (!swp) { break }
  }
}

// let arr = []
// sortBubble(arr)
// console.log(arr)
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// sortBubble(arr)
// console.log(arr)

// O(n^2), memory O(1), in-place
export function sortSelectIn(arr, cmp = (a, b) => a < b) {
  for (let i = 0; i < arr.length - 1; ++i) {
    let k = i
    for (let j = k + 1; j < arr.length; ++j) {
      if (cmp(arr[j], arr[k])) { k = j }
    }
    if (k !== i) { swap(arr, i, k) }
  }
}

// let arr = []
// sortSelect(arr)
// console.log(arr)
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// sortSelect(arr)
// console.log(arr)

// O(n^2), memory O(1), in-place
export function sortInsertIn(arr, cmp = (a, b) => a < b) {
  for (let i = 1; i < arr.length; ++i) {
    const el = arr[i]
    let j = i - 1
    while (j >= 0 && cmp(el, arr[j])) {
      arr[j + 1] = arr[j]
      --j
    }
    arr[j + 1] = el
  }
}

// O(n^2), memory O(1), in-place
function sortInsertIn2(arr, cmp = (a, b) => a < b) {
  for (let i = 1; i < arr.length; ++i) {
    let j = i
    while (j > 0 && cmp(arr[j], arr[j - 1])) {
      swap(arr, j - 1, j)
      --j
    }
  }
}

// let arr = []
// sortInsert(arr)
// console.log(arr)
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// sortInsert(arr)
// console.log(arr)

function merge(l, r, cmp) {
  const arr = []
  let i = 0
  let j = 0
  while (i < l.length && j < r.length) {
    if (cmp(l[i], r[j])) { arr.push(l[i]); ++i }
    else { arr.push(r[j]); ++j }
  }
  while (i < l.length) { arr.push(l[i]); ++i }
  while (j < r.length) { arr.push(r[j]); ++j }
  return arr
}

// O(n log n), memory O(n), returns a copy
export function sortMergeCp(arr, cmp = (a, b) => a < b) {
  if (arr.length <= 1) { return arr }
  // if (arr.length <= 3) { sortInsert(arr, cmp); return arr }
  const m = Math.floor(arr.length / 2)
  const l = sortMergeCp(arr.slice(0, m), cmp)
  const r = sortMergeCp(arr.slice(m), cmp)
  return merge(l, r, cmp)
}

// console.log(sortMerge([]))
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// console.log(sortMerge(arr))

// O(n log n), memory O(1), in-place
export function sortQuickIn(arr, cmp = (a, b) => a < b) {
  function sort(a, b) {
    if (a < b) {
      const p = arr[a + Math.floor((b - a) / 2)]
      let l = a
      let r = b
      while (l <= r) {
        while (cmp(arr[l], p)) { ++l }
        while (cmp(p, arr[r])) { --r }
        if (l <= r) { swap(arr, l, r); ++l; --r }
      }
      sort(a, r)
      sort(l, b)
    }
  }
  sort(0, arr.length - 1)
}

// let arr = []
// sortQuick(arr)
// console.log(arr)
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// sortQuick(arr)
// console.log(arr)

// O(n log n), memory O(log n), returns a copy
export function sortQuickCp(arr, cmp = lt) {
  if (arr.length <= 1) { return arr }
  const lt = []
  const eq = []
  const gt = []
  const p = arr[Math.floor(arr.length / 2)]
  for (const el of arr) {
    switch (cmp(el, p)) {
      case -1: lt.push(el); break
      case 1: gt.push(el); break
      default: eq.push(el)
    }
  }
  return [...sortQuickCp(lt, cmp), ...eq, ...sortQuickCp(gt, cmp)]
}

// console.log(sortQuick2([]))
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// console.log(sortQuick2(arr))

// O(n log n), memory O(n), returns a copy
export function sortHeapCp(arr, cmp = (a, b) => a < b) {
  const hp = Heap.from(arr, cmp)
  const res = []
  while (hp.length > 0) { res.push(hp.pop()) }
  return res
}

// console.log(sortHeap([]))
// let arr = [8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5]
// console.log(sortHeap(arr))

// ** Recursion

export function factorial(n) {
  let res = 1
  for (let i = 2; i <= n; ++i) { res *= i }
  return res
}

// for (const i of [0, 1, 2, 3, 4, 5, 6, 7]) { console.log(factorial(i)) }

function factorial2(n) {
  function factorial(n, res) {
    if (n < 2) { return res }
    return factorial(n - 1, res * n)
  }
  return factorial(n, 1)
}

// for (const i of [0, 1, 2, 3, 4, 5, 6, 7]) { console.log(factorial2(i)) }

// ** Dynamic programming

// O(n) minimal memoization
export function fibonacci(n) {
  if (n < 2) { return 1 }
  let a = 1
  let b = 1
  let c = a + b
  for (let i = 3; i <= n; ++i) {
    a = b; b = c; c = a + b
  }
  return c
}

// O(n) bottom-up (iterative)
function fibonacci2(n) {
  const mz = [1, 1]
  for (let i = 2; i <= n; ++i) {
    mz[i] = mz[i - 1] + mz[i - 2]
  }
  return mz[n]
}

// O(n) top-down (recursive)
function fibonacci3(n) {
  const mz = [1, 1]
  function fib(n) {
    if (n < mz.length) { return mz[n] }
    return mz[n] = fib(n - 1) + fib(n - 2)
  }
  return fib(n)
}

// O(2^n)
function fibonacci4(n) {
  if (n < 2) { return 1 }
  return fibonacci(n - 1) + fibonacci(n - 2)
}

// const arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
// for(const i of arr) { console.log(fibonacci(i)) }
// for(const i of arr) { console.log(fibonacci2(i)) }
// for(const i of arr) { console.log(fibonacci3(i)) }
// for(const i of arr) { console.log(fibonacci4(i)) }
