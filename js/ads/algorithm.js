import { Stack, BSTree } from "./dstructure.js"

function error(msg) { throw new Error(`ERROR: ${msg}`) }

// ** Array

export function sarFind(arr, vl) {
  function find(l, r) {
    // Stop on empty array
    if (l > r) { return -1 }
    const m = Math.floor(l + (r - l) / 2)
    // Element is found
    if (vl === arr[m]) { return m }
    // Binary search on a sorted array
    return vl < arr[m] ? find(l, m - 1) : find(m + 1, r)
  }
  return find(0, arr.length - 1)
}

// console.log(sarFind([], 99))
// const arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
// for (const vl of [8, 4, 2, 6, 0, 5, 99]) { console.log(sarFind(arr, vl)) }

// ** Stack

// O(n)
export function reverse(arr) {
  const st = Stack.from(arr)
  const rev = []
  while (st.length > 0) { rev.push(st.pop()) }
  return rev
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

// ** Sorting

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n^2), memory O(1) in-place
export function sortBubble(arr, cmp = (a, b) => a < b) {
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
// let arr = [8, 1, 4, 3, 2, 6, 7, 9, 0, 5]
// sortBubble(arr)
// console.log(arr)

// O(n^2), memory O(1) in-place
export function sortSelect(arr, cmp = (a, b) => a < b) {
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
// let arr = [8, 1, 4, 3, 2, 6, 7, 9, 0, 5]
// sortSelect(arr)
// console.log(arr)

// O(n^2), memory O(1) in-place
export function sortInsert(arr, cmp = (a, b) => a < b) {
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

// O(n^2), memory O(1) in-place
function sortInsert2(arr, cmp = (a, b) => a < b) {
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
// let arr = [8, 1, 4, 3, 2, 6, 7, 9, 0, 5]
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

// O(n log n), memory O(n) recursive
export function sortMerge(arr, cmp = (a, b) => a < b) {
  if (arr.length <= 1) { return arr }
  else {
    const m = Math.floor(arr.length / 2)
    const l = sortMerge(arr.slice(0, m), cmp)
    const r = sortMerge(arr.slice(m), cmp)
    return merge(l, r, cmp)
  }
}

// console.log(sortMerge([]))
// let arr = [8, 1, 4, 3, 2, 6, 7, 9, 0, 5]
// console.log(sortMerge(arr, (a, b) => a > b))
