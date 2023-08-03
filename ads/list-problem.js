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

// O(n) rearranges in-place a list so elements follow the low/high order
export function rearrangeLowHigh(lst) {
  const le = (a, b) => a < b, gt = (a, b) => a > b
  let curr = lst.head, cmp = le, i = 0
  while(curr?.next) {
    const next = curr.next
    if (!cmp(curr.data, next.data)) {
      [curr.data, next.data] = [next.data, curr.data]
    }
    cmp = ++i % 2 === 1 ? gt : le
    curr = curr.next
  }
}

const lst = List.from([5, 4, 3, 2, 1])
console.log(lst)
rearrangeLowHigh(lst)
console.log(lst)