import { Stack, TNode } from "./dstructure.js"

function error(msg) { throw new Error(msg) }

export function reverse(arr) {
  let res = []
  for (let i = arr.length - 1; i >= 0; --i) { res.push(arr[i]) }
  return res
}

export function reverse2(arr) {
  const st = Stack.from(arr)
  const rev = []
  while (st.length > 0) { rev.push(st.pop()) }
  return rev
}

// const arr = [1, 2, 3, 4, 5]
// console.log(reverse([]), reverse(arr))
// console.log(reverse2([]), reverse2(arr))

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

export function inOrder(nd, fn) {
  if (nd !== null) {
    inOrder(nd.left, fn)
    fn(nd.data)
    inOrder(nd.right, fn)
  }
}

export function preOrder(nd, fn) {
  if (nd !== null) {
    fn(nd.data)
    preOrder(nd.left, fn)
    preOrder(nd.right, fn)
  }
}

export function postOrder(nd, fn) {
  if (nd !== null) {
    postOrder(nd.left, fn)
    postOrder(nd.right, fn)
    fn(nd.data)
  }
}

// const tr = new TNode(1)
// tr.left = new TNode(2)
// tr.left.left = new TNode(3)
// tr.left.right = new TNode(4)
// tr.right = new TNode(5)
// tr.right.left = new TNode(6)
// tr.right.right = new TNode(7)
// console.log(tr)
// inOrder(tr, console.log)
// preOrder(tr, console.log)
// postOrder(tr, console.log)
