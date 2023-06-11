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
