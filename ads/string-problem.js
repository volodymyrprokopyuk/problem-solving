import { matrix } from "./array.js"
import { palindrome } from "./string.js"
import { HTable } from "./htable.js"

// O(n) returns positions of all overlapped anagrams of a word in a string
export function findAnagrams(word, str) {
  function freq(m, k, v) {
    if (m.get(k)) {
      const fr = m.get(k), val = fr + v
      if (val === 0) { m.delete(k) }
      else { m.set(k, val) }
    } else { m.set(k, v) }
  }
  const m = word.length, n = str.length
  const wf = new HTable(), sf = new HTable(), locs = []
  for (const ch of word) { freq(wf, ch, 1) }
  for (let r = 0, l = 0; r < n; ++r) {
    freq(sf, str[r], 1)
    if (r >= m) { freq(sf, str[l++], -1) }
    if (sf.equal(wf)) { locs.push(l) }
  }
  return locs
}

// O(n^2*m) returns pairs of positions that form a plindrome when concatenated
export function palindromePairs(arr) {
  const n = arr.length, pairs = []
  for (let i = 0; i < n; ++i) {
    for (let j = i + 1; j < n; ++j) {
      if (palindrome(arr[i] + arr[j])) { pairs.push([i, j]) }
      if (palindrome(arr[j] + arr[i])) { pairs.push([j, i]) }
    }
  }
  return pairs
}

// O(n) prints a string in zig zag of height k
export function printZigZag(str, k) {
  const arr = str.split(""), n = arr.length, m = k - 1, mx = matrix(k, n, " ")
  let down = false
  for (let j = 0; j < n; ++j) {
    if (j % m === 0) { down = !down }
    const i = down ? j % m : m - j % m
    mx[i][j] = arr[j]
  }
  return mx
}
