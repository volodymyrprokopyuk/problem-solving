// O(1) swaps in-place two array elements
export function swap(arr, i, j) {
  [arr[i], arr[j]] = [arr[j], arr[i]]
}

// O(m*n) creates mxn matrix optionally filled with v
export function matrix(m, n = m, v) {
  return Array(m).fill().map(_ => Array(n).fill(v))
}
