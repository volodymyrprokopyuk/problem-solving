import { HSet } from "./htable.js"

// O(n) returns all pairs that sum to k
export function twoSum(arr, k) {
  const set = new HSet(), pairs = []
  for (const el of arr) {
    const diff = k - el
    if (set.get(diff)) { pairs.push([el, diff]) }
    set.set(el)
  }
  return pairs
}
