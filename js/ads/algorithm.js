import { Stack, BSTree } from "./dstructure.js"

function error(msg) { throw new Error(`ERROR: ${msg}`) }

// ** Stack

// O(n)
export function reverse(arr) {
  const st = Stack.from(arr)
  const rev = []
  while (st.length > 0) { rev.push(st.pop()) }
  return rev
}

// const arr = [1, 2, 3, 4, 5]
// console.log(reverse([]), reverse(arr))

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
