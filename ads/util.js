// test: npx vitest run --reporter verbose --coverage -t merge

// throws an error
export function error(message) {
  throw new Error(message)
}

// O(1) swaps in-place two array elements
export function arrSwap(arr, i, j) {
  [arr[i], arr[j]] = [arr[j], arr[i]]
}

// O(m*n) creates an mxn matrix optionally filled with v
export function matrix(m, n = m, v) {
  return Array(m).fill().map(_ => Array(n).fill(v))
}

// O(n) returns true if maps are equal, false otherwise
export function mapEq(a, b) {
  if (a.size !== b.size) { return false }
  for (const [k, v] of a) {
    if (v !== b.get(k)) { return false }
  }
  return true
}

// O(n) non-cryptographic hash function
export function djb2(str) {
  let hash = 5381
  for (const ch of String(str)) {
    hash = hash * 33 + ch.charCodeAt(0)
  }
  return hash
}
