function lt(a, b) { return a < b }
function gt(a, b) { return a > b }

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n^2), in-place, stable
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

// O(n^2), in-place, not statlbe
export function selectionSort(arr, cmp = lt) {
  for (let i = 0; i < arr.length - 1; ++i) {
    let k = i
    for (let j = k + 1; j < arr.length; ++j) {
      if (cmp(arr[j], arr[k])) { k = j }
    }
    if (k !== i) { swap(arr, i, k) }
  }
}

// O(n^2), in-place, stable
export function insertionSort(arr, cmp = lt) {
  for (let i = 1; i < arr.length; ++i) {
    let j = i
    while (j > 0 && cmp(arr[j], arr[j - 1])) {
      swap(arr, j, j - 1)
      --j
    }
  }
}

// O(n*log(n)), copy, stable
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

// O(n*log(n)), in-place, not stable
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

const arrs = [
  [], [0], [1, 2],
  [3, 2, 1],
  [9, 0, 2, 4, 6, 3, 8, 9, 7, 1, 5, 0]
]

arrs.forEach(arr => { quickSort(arr, gt); console.log(arr) })
// arrs.forEach(arr => { console.log(mergeSort(arr)) })
