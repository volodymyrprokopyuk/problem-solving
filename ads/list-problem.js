import { List } from "./list.js"

// O(m + n) adds two number represented as linked list of digits
export function addNumbers(a, b) {
  function add(a, b) {
    const sum = (a && b ? a.pop() + b.pop() : a.pop()) + quot
    quot = Math.floor(sum / 10)
    c.push(sum % 10)
  }
  const c = new List()
  let quot = 0
  while (a.length > 0 && b.length > 0) { add(a, b) }
  while (a.length > 0) { add(a) }
  while (b.length > 0) { add(b) }
  if (quot !== 0) { c.push(quot) }
  c.reverse()
  return c
}
