import { swap } from "./util.js"

// O(m+n) merges two sorted arrays
export function merge(a, b) {
  const m = a.length, n = b.length, c = []
  let i = 0, j = 0
  while (i < m && j < n) {
    c.push(a[i] < b[j] ? a[i++] : b[j++])
  }
  while (i < m) { c.push(a[i++]) }
  while (j < n) { c.push(b[j++]) }
  return c
}

// O(n) partitions in-place an array and return a pivot index
export function hoarePartition(arr, l = 0, r = arr.length - 1) {
  if (r < 0) { return }
  const p = l++
  while (true) {
    while (arr[l] <= arr[p]) { ++l }
    while (arr[p] < arr[r]) { --r }
    if (l < r) { swap(arr, l, r); ++l; --r }
    else { swap(arr, p, r); return r }
  }
}

[[], [1], [1, 2], [2, 1], [2, 3, 1], [2, 1, 3, 2], [5, 9, 1, 3, 7, 8, 4, 6, 2]
].forEach(arr => {
  console.log(arr)
  console.log(arr, hoarePartition(arr))
})
