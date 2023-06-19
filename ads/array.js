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

// O(n) partitions in-place an array and returns a pivot index
export function hoarePartition(arr, l = 0, r = arr.length - 1) {
  if (r < 0) { return }
  const p = l++ // pivot = first
  while (true) {
    while (arr[l] <= arr[p]) { ++l }
    while (arr[p] < arr[r]) { --r }
    if (l < r) { swap(arr, l, r); ++l; --r }
    else { swap(arr, p, r); return r }
  }
}

// O(n) partitions in-place an array and returns a pivot index
export function lomutoPartition(arr, l = 0, r = arr.length - 1) {
  if (r < 0) { return }
  let i = l // pivot = last
  for (let j = l; j < r; ++j) {
    if (arr[j] <= arr[r]) { swap(arr, i++, j) }
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
