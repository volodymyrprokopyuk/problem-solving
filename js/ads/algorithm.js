import { Stack, BSTree } from "./dstructure.js"

function error(msg) { throw new Error(msg) }

// ** Array

// O(n)
export function reverse(arr) {
  let res = []
  for (let i = arr.length - 1; i >= 0; --i) { res.push(arr[i]) }
  return res
}

// O(n)
export function reverse2(arr) {
  const st = Stack.from(arr)
  const rev = []
  while (st.length > 0) { rev.push(st.pop()) }
  return rev
}

// const arr = [1, 2, 3, 4, 5]
// console.log(reverse([]), reverse(arr))
// console.log(reverse2([]), reverse2(arr))

// ** Stack

// O(n)
export function checkParens(
  str, parens = { "(": ")", "[": "]", "{": "}", "<": ">" }
) {
  const cl = new Set(Object.values(parens))
  const st = new Stack()
  for (const ch of str) {
    if (ch in parens) { st.push(ch) }
    else if (cl.has(ch)) {
      if (st.length === 0) { error(`extra close ${ch}`) }
      const op = st.pop()
      if (ch !== parens[op]) { error(`mismatch ${op} ${ch}`) }
    }
  }
  if (st.length !== 0) { error(`extra open ${st.peek()}`) }
  return true
}

// const strs = ["", "abc", "a(b{c[d<e>f]g}h)i", "a(", "a(b]", "abc)"]
// for (const str of strs) {
//   try { console.log(checkParens(str)) }
//   catch (err) { console.error(`ERROR: ${err.message}`) }
// }

// ** Tree

// O(n)
export function bstInOrder(nd, fn) {
  if (nd !== null) {
    bstInOrder(nd.left, fn)
    // Node in the middle
    fn(nd)
    bstInOrder(nd.right, fn)
  }
}

// O(n)
export function bstPreOrder(nd, fn) {
  if (nd !== null) {
    // Node first
    fn(nd)
    bstPreOrder(nd.left, fn)
    bstPreOrder(nd.right, fn)
  }
}

// O(n)
export function bstPostOrder(nd, fn) {
  if (nd !== null) {
    bstPostOrder(nd.left, fn)
    bstPostOrder(nd.right, fn)
    // Node last
    fn(nd)
  }
}

// const tr = BSTree.from([3, 1, 4, 5, 2])
// let arr = []
// bstInOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)
// arr = []
// bstPreOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)
// arr = []
// bstPostOrder(tr.root, nd => arr.push(nd.data))
// console.log(arr)

// O(log n)
export function bstFind(nd, vl) {
  if (nd === null) { return null }
  // Element is found
  if (vl === nd.data) { return nd }
  // Binary search
  return vl < nd.data ? bstFind(nd.left, vl) : bstFind(nd.right, vl)
}

// O(log n)
export function bstMin(nd) {
  if (nd === null) { error("bstMin on null") }
  // Minimum is the leftmost child node
  return nd.left === null ? nd : bstMin(nd.left)
}

// O(log n)
export function bstMax(nd) {
  if (nd === null) { error("bstMax on null") }
  // Maximum is the rightmost child node
  return nd.right === null ? nd : bstMax(nd.right)
}

// const tr = BSTree.from([8, 1, 4, 3, 2, 6, 7, 9, 0, 5])
// console.log(bstFind(tr.root, 1)?.data, bstFind(tr.root, -1)?.data)
// console.log(bstMin(tr.root)?.data, bstMax(tr.root)?.data)

// O(log n)
export function bstFindParent(nd, vl) {
  // No parent for the null or the root itself
  if (nd === null || vl === nd.data) { return null }
  if (vl < nd.data) {
    if (nd.left === null) { return null }
    // Match on the left, otherwise proceed further down the left path
    return vl === nd.left.data ? nd : bstFindParent(nd.left, vl)
  } else {
    if (nd.right === null) { return null }
    // Match on the right, otherwise proceed further down the right path
    return vl === nd.right.data ? nd : bstFindParent(nd.right, vl)
  }
}

// const tr = BSTree.from([3, 1, 4, 5, 2])
// console.log(bstFindParent(tr.root, 3)?.data)
// console.log(bstFindParent(tr.root, 1)?.data)
// console.log(bstFindParent(tr.root, 4)?.data)
// console.log(bstFindParent(tr.root, 2)?.data)
// console.log(bstFindParent(tr.root, 5)?.data)
