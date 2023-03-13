import { Stack, BSTree } from "./dstructure.js"

function error(msg) { throw new Error(`ERROR: ${msg}`) }

// ** Array

export function isPermut(a, b) {
  const ast = new Set()
  const bst = new Set()
  for (const el in a) { ast.add(el) }
  for (const el in b) { bst.add(el) }
  return ast === bst
}

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
//   catch (err) { console.error(err.message) }
// }

// ** Hash

/* ** Two-sum problem
 * Given a list of numbers and a number k, return whether any two numbers from
 * the list add up to k
 */

// O(n^2) - sums only distinct numbers
export function twoSum(arr, k) {
  for (let i = 0; i < arr.length; ++i) {
    for (let j = 0; j < arr.length; ++j) {
      if (i !== j && arr[i] + arr[j] === k) { return true }
    }
  }
  return false
}

// console.log(twoSum([10, 15, 3, 7], 20))

// O(n) - sums also the number to itself
export function twoSum2(arr, k) {
  const ht = { }
  for (const el of arr) {
    ht[el] = true
    if (k - el in ht) { return true }
  }
  return false
}

// console.log(twoSum2([10, 15, 3, 7], 20))

// ** Tree

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

// ** PowerSet

// O(3^n)
function keypadWords(digits) {
  const tr = new Map()
  tr.set(2, ["a", "b", "c"])
  tr.set(3, ["d", "e", "f"])
  tr.set(9, ["w", "x", "y", "z"])
  let words = [[]]
  for (const digit of digits) {
    const letters = tr.get(digit)
    const newWords = []
    for (const word of words) {
      for (const letter of letters) {
        newWords.push(word.concat(letter))
      }
    }
    words = newWords
  }
  return words
}
console.log(keypadWords([2, 3, 9]))
