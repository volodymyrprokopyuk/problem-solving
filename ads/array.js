import { DList } from "./list.js"
import { Heap } from "./heap.js"
import { BSTree, inOrder } from "./tree.js"

// O(m*n) creates an mxn matrix optionally filled with v
export function matrix(m, n = m, v) {
  return Array(m).fill().map(_ => Array(n).fill(v))
}

// O(1) swaps in-place two array elements
export function swap(arr, i, j) {
  [arr[i], arr[j]] = [arr[j], arr[i]]
}

// O(m+n) merges two sorted arrays
export function merge(a, b, cmp = (a, b) => a < b) {
  const m = a.length, n = b.length, c = []
  let i = 0, j = 0
  while (i < m && j < n) {
    c.push(cmp(a[i], b[j]) ? a[i++] : b[j++])
  }
  while (i < m) { c.push(a[i++]) }
  while (j < n) { c.push(b[j++]) }
  return c
}

// O(n) partitions in-place an array and returns a pivot index
export function hoarePartition(
  arr, l = 0, r = arr.length - 1, cmp = (a, b) => a < b
) {
  if (r < 0) { return }
  const p = l++ // pivot = first
  while (true) {
    while (cmp(arr[l], arr[p])) { ++l }
    while (cmp(arr[p], arr[r])) { --r }
    if (l < r) { swap(arr, l, r); ++l; --r }
    else { swap(arr, p, r); return r }
  }
}

// O(n) partitions in-place an array and returns a pivot index
export function lomutoPartition(
  arr, l = 0, r = arr.length - 1, cmp = (a, b) => a < b
) {
  if (r < 0) { return }
  let i = l // pivot = last
  for (let j = l; j < r; ++j) {
    if (cmp(arr[j], arr[r])) { swap(arr, i++, j) }
  }
  swap(arr, i, r)
  return i
}

// O(n) returns the index of the element in the k-th position of a sorted array
export function quickSelect(arr, k, l = 0, r = arr.length - 1) {
  if (l > r || k > r) { return }
  const p = hoarePartition(arr, l, r)
  // const p = lomutoPartition(arr, l, r)
  return p < k ? quickSelect(arr, k, p + 1, r) :
    p > k ? quickSelect(arr, k, l, p - 1) : p
}

// O(n!) returns all permunations of an array
export function permutations(arr) {
  if (arr.length === 0) { return [[]] }
  const perms = []
  for (const el of arr) {
    const ps = permutations(arr.filter(e => e !== el))
    for (const p of ps) { perms.push([el, ...p]) }
  }
  return perms
}

// O(n!) returns all permunations of an array
export function permutations2(arr) {
  if (arr.length === 0) { return [[]] }
  return arr.flatMap(el =>
    permutations2(arr.filter(e => e !== el)).map(p => [el, ...p]))
}

// O(2^n) bottom-up, iterative
export function powerset(arr) {
  const set = [[]]
  for (const el of arr) {
    for (let i = 0, len = set.length; i < len; ++i) {
      set.push([...set[i], el])
    }
  }
  return set
}

// O(2^n) top-down, recursive
export function powerset2([h, ...tl]) {
  if (!h) { return [[]] }
  const set = powerset2(tl)
  return [...set, ...set.map(s => [h, ...s])]
}

// O(log(n)) returns a position of value in an array, otherwise -1
export function binarySearch(val, arr) {
  let a = 0, b = arr.length - 1
  while (a <= b) {
    const m = Math.floor((a + b) / 2)
    if (val < arr[m]) { b = m - 1 }
    else if (val > arr[m]) { a = m + 1 }
    else { return m }
  }
  return -1
}

// O(n^2) sorts an array in place
export function bubbleSort(arr, cmp = (a, b) => a < b) {
  for (let i = arr.length - 1; i > 0; --i) {
    let swp = false
    for (let j = 0; j < i; ++j) {
      if (cmp(arr[j + 1], arr[j])) {
        swap(arr, j, j + 1); swp = true
      }
    }
    if (!swp) { break }
  }
}

// O(n^2) sorts an array in place
export function insertionSort(arr, cmp = (a, b) => a < b) {
  for (let i = 1; i < arr.length; ++i) {
    const el = arr[i]
    let j = i
    while (j > 0 && cmp(el, arr[j - 1])) {
      arr[j] = arr[j - 1]; --j
    }
    if (j !== i) { arr[j] = el }
  }
}

// O(n^2) sorts an array in place
export function insertionSort2(arr, cmp = (a, b) => a < b) {
  for (let i = 1; i < arr.length; ++i) {
    let j = i
    while (j > 0 && cmp(arr[j], arr[j - 1])) {
      swap(arr, j, j - 1); --j
    }
  }
}

// O(n*log(n)) sorts an array in-place
export function shellSort(arr, cmp = (a, b) => a < b) {
  const gaps = [31, 15, 7, 3, 1]
  for (const gap of gaps) {
    for (let i = gap; i < arr.length; ++i) {
      let j = i
      while (j > 0 && cmp(arr[j], arr[j - gap])) {
        swap(arr, j, j - gap); --j
      }
    }
  }
}

// O(n^2) sorts an array in place
export function selectionSort(arr, cmp = (a, b) => a < b) {
  for (let i = 0; i < arr.length - 1; ++i) {
    let m = i
    for (let j = i + 1; j < arr.length; ++j) {
      if (cmp(arr[j], arr[m])) { m = j }
    }
    if (m !== i) { swap(arr, i, m) }
  }
}

// O(n*log(n)) sorts an array, returns a copy
export function mergeSort(arr, cmp = (a, b) => a < b) {
  if (arr.length < 2) { return arr }
  const m = Math.floor(arr.length / 2),
        a = mergeSort(arr.slice(0, m), cmp),
        b = mergeSort(arr.slice(m), cmp)
  return merge(a, b, cmp)
}

// O(n*log(n)) sorts an array in-place
export function quickSort(
  arr, l = 0, r = arr.length - 1, cmp = (a, b) => a < b
) {
  if (l < r) {
    const p = hoarePartition(arr, l, r, cmp)
    quickSort(arr, l, p - 1, cmp)
    quickSort(arr, p + 1, r, cmp)
  }
}

// O(n*log(n)) sorts an array, returns a copy
export function quickSort2(arr, cmp = (a, b) => a < b) {
  if (arr.length < 2) { return arr }
  const p = hoarePartition(arr, 0, arr.length - 1, cmp),
        l = quickSort2(arr.slice(0, p), cmp),
        r = quickSort2(arr.slice(p + 1), cmp)
  return [...l, arr[p], ...r]
}

// O(n*log(n)) sorts an array, returns a copy
export function heapSort(arr, cmp = (a, b) => a < b) {
  return [...Heap.from(arr, cmp)]
}

// O(n*log(n)) sorts an array, returns a copy
export function bstSort(arr, cmp = (a, b) => a < b) {
  const bst = BSTree.from(arr, cmp)
  return [...inOrder(bst.root, true)]
}

// O(digits/letters.lenght*n) sorts an array of numbers, returns a copy
export function radixSortNum(arr) {
  function* collect(bkts) {
    for (const bkt of bkts) { yield* bkt }
  }
  const digits = Math.ceil(Math.log10(Math.max(...arr) + 1))
  for (let k = 0; k < digits; ++k) {
    const bkts = Array(10).fill().map(_ => new DList())
    for (const num of arr) {
      const i = Math.floor(num / 10 ** k) % 10
      bkts[i].pushTail(num)
    }
    arr = [...collect(bkts)]
  }
  return arr
}
