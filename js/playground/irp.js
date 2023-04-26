import { readdir } from "fs/promises"
import path from "path"

const eq = (a, b) => a === b
const lt = (a, b) => a < b
const le = (a, b) => a <= b
const gt = (a, b) => a > b
const ge = (a, b) => a >= b

function error(msg) { throw new Error(msg) }

function swap(arr, i, j) {
  [arr[i], arr[j]] = [arr[j], arr[i]]
}

// O(n)
function sumFirstN(n) {
  return n === 0 ? 0 : sumFirstN(n - 1) + n
}

// O(n) tail-recursive
function sumFirstN2(n, sum = 0) {
  return n === 0 ? sum : sumFirstN2(n - 1, sum + n)
}

// for (const i of Array(6).keys()) { console.log(sumFirstN2(i)) }

// O(n) iterative
function* fibonacci(n) {
  let a = -1, b = 1
  while (n-- > 0) {
    const c = a + b
    a = b, b = c
    yield c
  }
}

// console.log(...fibonacci(10))

// O(n) memoization
function fibonacci2(n, cache = new Map()) {
  if (n < 2) { return n }
  if (cache.has(n)) { return cache.get(n) }
  cache.set(n, fibonacci2(n - 1, cache) + fibonacci2(n - 2, cache))
  return cache.get(n)
}

// for (const i of Array(10).keys()) { console.log(fibonacci2(i)) }

// o(n)
function power(x, n) {
  return n === 0 ? 1 :
    n > 0 ? power(x, n - 1) * x :
    power(x, n + 1) / x
}

// [-4, -3, -2, -1, 0, 1, 2, 3, 4].forEach(n => console.log(power(2, n)))

// o(log(n))
function power2(x, n) {
  return n === 0 ? 1 :
    n % 2 === 0 ? power2(x, n / 2) ** 2 :
    power2(x, (n - 1) / 2) ** 2 * x
}

// [0, 1, 2, 3, 4].forEach(n => console.log(power2(2, n)))

// O(n)
function add(a, b) {
  return a === 0 ? b : b === 0 ? a : add(a - 1, b) + 1
}

// [[0, 1], [2, 3]].forEach(([a, b]) => console.log(add(a, b)))

// O(n)
function toBase(x, base = 2) {
  return x < base ? x :
    toBase(Math.floor(x / base), base) * 10 + x % base
}

// [0, 1, 2, 9, 15].forEach(x => console.log(toBase(x)));
// [47, 142].forEach(n => console.log(toBase(n, 5)))

// O(n)
function reverseString(s) {
  return s.length === 0 ? "" : reverseString(s.slice(1)) + s[0]
}

// ["", "a", "abc"].forEach(s => console.log(reverseString(s)))

// O(n)
function isPalindrome(s) {
  return s.length < 2 ? true :
    s.at(0) === s.at(-1) && isPalindrome(s.slice(1, -1))
}

// ["", "a", "ab", "abba", "abc", "abcba"].forEach(s =>
//   console.log(isPalindrome(s))
// )

// O(n^2)
function selectionSort(arr, cmp = lt) {
  if (arr.length < 2) { return arr }
  const m = arr.reduce((m, _, i) => cmp(arr[i], arr[m]) ? i : m, 0)
  if (m !== 0) { swap(arr, 0, m) }
  return [arr[0], ...selectionSort(arr.slice(1), cmp)]
}

// [[], [1], [2, 3], [6, 4, 0, 1, 5, 0, 3, 2]].forEach(arr =>
//   console.log(selectionSort(arr, gt))
// )

// O(n)
function hornerPolynomial(arr, x) {
  return arr.length === 1 ? arr[0] :
    hornerPolynomial(arr.slice(1), x) * x + arr[0]
}

// [[1], [1, 2], [1, 1, 2, 3]].forEach(arr =>
//   console.log(hornerPolynomial(arr, 2))
// )

// O(n)
function containsDigit(n, d) {
  return n < 10 ? n === d :
    n % 10 === d || containsDigit(Math.floor(n / 10), d) // shortcircuit
}

// O(n) tail-recursive
function containsDigit2(n, d) {
  return n < 10 ? n === d :
    n % 10 === d ? true :
    containsDigit2(Math.floor(n / 10), d)
}

// [1, 12, 123].forEach(n => console.log(containsDigit2(n, 2)))

// O(n)
function equalString(a, b) {
  return a.length !== b.length ? false :
    a === "" ? true :
    a[0] === b[0] && equalString(a.slice(1), b.slice(1)) // shortcircuit
}

// O(n) tail-recursive
function equalString2(a, b) {
  return a.length !== b.length ? false :
    a === "" ? true :
    a[0] !== b[0] ? false :
    equalString2(a.slice(1), b.slice(1))
}

// [["", ""], ["", "a"], ["a", "b"], ["abc", "abc"]].forEach(([a, b]) =>
//   console.log(a, b, equalString2(a, b))
// )

// O(n) tail-recursive
function linearSearch(arr, x, i = 0) {
  return i === arr.length ? -1 :
    arr[i] === x ? i :
    linearSearch(arr, x, ++i)
}

// [[[], 1], [[1], 1], [[1, 2, 3], 2], [[1, 2], 3]].forEach(([arr, x]) =>
//   console.log(linearSearch(arr, x))
// )

// O(log(n)) tail-recursive
function binarySearch(arr, x, a = 0, b = arr.length - 1) {
  if (b < a) { return -1 }
  const m = Math.floor((a + b) / 2)
  return arr[m] === x ? m :
    x < arr[m] ? binarySearch(arr, x, a, m - 1) :
    binarySearch(arr, x, m + 1, b)
}

// [[[], 1], [[1, 2], 3], [[1, 2, 3, 4], 2]].forEach(([arr, x]) =>
//   console.log(binarySearch(arr, x))
// )

class TNode {
  key; data
  left = null; right = null

  constructor(key, data) {
    this.key = key; this.data = data
  }
}

class BSTree {
  #root; length = 0

  // O(log(n)) tail-recursive
  set(key, data, nd = this.#root) {
    if (this.length === 0) {
      this.#root = new TNode(key, data); ++this.length
    } else if (key < nd.key) {
      if (!nd.left) {
        nd.left = new TNode(key, data); ++this.length
      } else { this.set(key, data, nd.left) }
    } else {
      if (!nd.right) {
        nd.right = new TNode(key, data); ++this.length
      } else { this.set(key, data, nd.right) }
    }
  }

  // O(log(n)) tail-recursive
  get(key, nd = this.#root) {
    if (nd) {
      return nd.key === key ? nd :
        key < nd.key ? this.get(key, nd.left) :
        this.get(key, nd.right)
    }
  }

  get inOrder() {
    function* inOrderGen(nd) {
      if (nd) {
        yield* inOrderGen(nd.left)
        yield nd
        yield* inOrderGen(nd.right)
      }
    }
    return { [Symbol.iterator]: () => inOrderGen(this.#root) }
  }
}

// const tr = new BSTree();
// [5, 2, 3, 6, 1, 4].forEach(key => tr.set(key, key.toString()))
// for (const nd of tr.inOrder) { console.log(nd.key, nd.data) }
// [1, 4, 6, 2, 5, 0].forEach(key => console.log(tr.get(key)))

// O(n)
function hoarePartition(arr, l = 0, r = arr.length - 1) {
  if (r < l) { return Array(2) }
  if (l === r) { return [arr[r], r] }
  if (l === r - 1) {
    if (arr[r] < arr[l]) { swap(arr, l, r) }
    return [arr[r], r]
  }
  let p = Math.floor((l + r) / 2)
  swap(arr, l, p); p = l; ++l
  while (true) {
    while (arr[l] <= arr[p]) { ++l }
    while (arr[p] < arr[r]) { --r }
    if (l < r) { swap(arr, l, r); ++l; --r }
    else { swap(arr, p, r); return [arr[r], r] }
  }
}

// O(n) tail-recursive
function quickSelect(arr, k, l = 0, r = arr.length - 1) {
  if (arr.length === 0 || arr.length < k) { return }
  const [p, i] = hoarePartition(arr, l, r)
  return i === k - 1 ? p :
    i < k - 1 ? quickSelect(arr, k, i + 1, r) :
    quickSelect(arr, k, l, i - 1)
}

// [[], [1], [1, 2], [2, 1], [1, 2, 3], [3, 2, 1], [1, 1, 1],
//  [3, 1, 2, 4, 5, 4],
//  [7, 2, 1, 4, 3, 6, 4, 8, 5, 9, 0],
//  [2, 1, 3, 0, 4, 9, 5, 6, 10, 7, 8]
// ].forEach(arr =>
//   // console.log(arr, hoarePartition(arr))
//   console.log(arr, quickSelect(arr, 6))
// )

// O(b - a) tail-recursive
function bisection(f, a, b, e = 1e-5) {
  const z = (a + b) / 2, fz = f(z), fa = f(a)
  return b - a <= 2 * e ? z :
    fz > 0 && fa > 0 || fz < 0 && fa < 0 ?
    bisection(f, z, b, e) : bisection(f, a, z, e)
}

// console.log(bisection(x => x ** 2 - 2, 0, 4))

// O(n) tail-recursive
function countWood(trees, height, count = 0) {
  return trees.length === 0 ? count :
    countWood(
      trees.slice(1), height,
      trees[0] > height ? count + trees[0] - height : count
    )
}

// O(n*log(h)) tail-recursive
function collectWood(trees, wood, lower = 0, upper = Math.max(...trees)) {
  const height = Math.floor((lower + upper) / 2)
  const count = countWood(trees, height)
  if (count === wood || lower === upper) { return height }
  if (lower === upper - 1) {
    return countWood(trees, upper) >= wood ? upper : lower
  }
  return count < wood ?
    collectWood(trees, wood, lower, height - 1) :
    collectWood(trees, wood, height, upper)
}

// const trees = [5, 4, 3, 12, 8, 7, 5, 10, 7, 8, 4, 4, 11, 8, 7, 1, 9, 4, 3, 5];
// [6, 7, 8, 9, 10].forEach(wood => console.log(collectWood(trees, wood)))

// tail-recursive
function gcd(a, b) {
  return a === 0 ? b : b === 0 ? a :
    a > b ? gcd(b, a % b) : gcd(a, b % a)
}

// iterative
function gcd2(a, b) {
  while (true) {
    if (a === 0) { return b }
    if (b === 0) { return a }
    if (a > b) { a = a % b } else { b = b % a }
  }
}

// [[1, 2], [20, 24], [20, 17]].forEach(([a, b]) => console.log(gcd2(a, b)))

// O(n)
function isSorted(arr, cmp = lt) {
  const m = Math.floor(arr.length / 2)
  return arr.length < 2 ? true :
    isSorted(arr.slice(0, m), cmp) &&
    cmp(arr[m - 1], arr[m]) &&
    isSorted(arr.slice(m), cmp)
}

// [[], [1], [1, 2], [2, 1], [1, 2, 30, 40], [1, 2, 3, 5, 4]].forEach(arr =>
//   console.log(arr, isSorted(arr))
// )

// O(n)
function merge(a, b) {
  return a.length === 0 ? b :
    b.length === 0 ? a :
    a[0] < b[0] ? [a[0], ...merge(a.slice(1), b)] :
    [b[0], ...merge(a, b.slice(1))]
}

// O(n*log(n)) out-of-place
function mergeSort(a) {
  let m = Math.floor(a.length / 2)
  return a.length < 2 ? a :
    merge(mergeSort(a.slice(0, m)), mergeSort(a.slice(m)))
}

// [[], [], [1, 2], [3, 2, 1],
//  [2, 1, 3, 0, 4, 9, 5, 4, 6, 10, 7, 8]].forEach(arr =>
//   console.log(mergeSort(arr))
// )

// O(n*log(n)) in-place
function quickSort(arr, l = 0, r = arr.length - 1) {
  if (l < r) {
    const [p, i] = hoarePartition(arr, l, r)
    quickSort(arr, l, i - 1)
    quickSort(arr, i + 1, r)
  }
}

// [[], [1], [1, 2], [2, 1], [1, 2, 3], [3, 2, 1], [1, 1, 1],
//  [3, 1, 2, 4, 5, 4],
//  [7, 2, 1, 4, 3, 6, 4, 8, 5, 9, 0],
//  [2, 1, 3, 0, 4, 9, 5, 6, 10, 7, 8]
// ].forEach(arr => { quickSort(arr); console.log(arr) })

// O(n*log(n))
function majorityElement(arr) {
  function count(arr, val) {
    return arr.reduce((cnt, el) => el === val ? cnt + 1 : cnt, 0)
  }
  if (arr.length === 0) { return [false, undefined, 0] }
  if (arr.length === 1) { return [true, arr[0], 1] }
  const m = Math.floor(arr.length / 2)
  const l = arr.slice(0, m)
  const r = arr.slice(m)
  const [lm, le, lc] = majorityElement(l)
  if (lm) {
    const rc = count(r, le)
    if (lc + rc > m) { return [true, le, lc + rc] }
  }
  const [rm, re, rc] = majorityElement(r)
  if (rm) {
    const lc = count(l, re)
    if (rc + lc > m) { return [true, re, rc + lc] }
  }
  return [false, undefined, 0]
}

// [[4, 4, 5, 1, 4, 2, 4, 3], [4, 4, 5, 4, 1, 2, 4, 3],
//  [2, 4, 3, 4, 4, 1, 4, 4]].forEach(arr => console.log(majorityElement(arr, 4)))

// O(2^n)
function towersOfHanoi(n, o = "Org", d = "Dst", a = "Aux") {
  if (n > 0) {
    towersOfHanoi(n - 1, o, a, d)
    console.log(`${n}: ${o} => ${d}`)
    towersOfHanoi(n - 1, a, d, o)
  }
}

// towersOfHanoi(3)

// O(2^n)
function longestPalindrome(s) {
  if (isPalindrome(s)) { return s }
  const a = longestPalindrome(s.slice(1))
  const b = longestPalindrome(s.slice(0, -1))
  return a.length > b.length ? a : b
}

// ["", "a", "aba", "abccb", "abac"].forEach(str =>
//   console.log(str, longestPalindrome(str))
// )

// O(n) mutually-recursive
function isEven(n) {
  return n === 0 ? true : isOdd(n - 1)
}

// O(n) mutually-recursive
function isOdd(n) {
  return n === 0 ? false : isEven(n - 1)
}

// [1, 2, 3, 4, 5].forEach(n => console.log(n, isEven(n), isOdd(n)))

// O(n) tail-recursive
function calcTokenize(s, tk = []) {
  if (s.length === 0) { return tk }
  let m = s.match(/^ +/)
  if (m) { return calcTokenize(s.slice(m[0].length), tk) }
  m = s.match(/^(?:-?\d+|[-+*\/\(\)])/)
  if (m) {
    tk.push(m[0])
    return calcTokenize(s.slice(m[0].length), tk)
  } else { error(`invalid expression: ${s}`) }
}

// [
//   "-(-6 / 3) - (-(4)) + (18 - 2)",
//   "-(2 + 3) + 2 * 8 / 4 - 3 * (4 + 2) / 6 + 7 - (-(2 + 8) * 3)",
// ].forEach(expr =>
//   console.log(calcTokenize(expr))
// )

// Iterative + explicit stack
async function searchFile(file, root) {
  async function searchDir(dir) {
    for (const entry of await readdir(dir, { withFileTypes: true })) {
      const epath = path.join(dir, entry.name)
      if (entry.isFile() && entry.name === file ) { console.log(epath) }
      if (entry.isDirectory()) { st.push(epath) } // explicit stack
    }
  }
  const st = [root]
  while (st.length !== 0) { await searchDir(st.pop()) }
}

// Recursive + implicit stack
async function searchFile2(file, dir) {
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const epath = path.join(dir, entry.name)
    if (entry.isFile() && entry.name === file) { console.log(epath) }
    if (entry.isDirectory()) { searchFile2(file, epath) } // implicit stack
  }
}

// await searchFile2("irp.js", "../")

// O(n) tail-recursive
function digitalRoot(n) {
  function dsum(n) {
    let s = 0
    while (n > 9) {
      s += n % 10
      n = Math.floor(n / 10)
    }
    return s + n
  }
  return n < 10 ? n : digitalRoot(dsum(n))
}

// O(n) nested recursion
function digitalRoot2(n) {
  return n < 10 ? n :
    digitalRoot2(digitalRoot2(Math.floor(n / 10)) + n % 10)
}

// [0, 1, 12, 123, 79868].forEach(n => console.log(digitalRoot2(n)))

// Backtracking + dynamic multiple recursion
function nqueens(
  n, col = 0, rs = Array(n).fill(true),
  pds = Array(2 * n - 1).fill(true),
  sds = Array(2 * n - 1).fill(true),
  queens = Array(n)
) {
  // Complete solution: all n quens are placed in correct positions
  if (col === n) { console.log(queens) }
  else {
    // Generate all possible candidates
    for (const cnd of Array(n).keys()) {
      // Check for free row, free primary and sencondary diagonals
      if (rs[cnd] && pds[col - cnd + n - 1] && sds[col + cnd]) {
        queens[col] = cnd // Place a candidate queen
        // Mark the candiate position as occupied
        rs[cnd] = pds[col - cnd + n - 1] = sds[col + cnd] = false
        // Extend the paritial solution with more candidates
        nqueens(n, col + 1, rs, pds, sds, queens)
        // Backtracking: mark the candiate position as free
        rs[cnd] = pds[col - cnd + n - 1] = sds[col + cnd] = true
      }
    }
  }
}

// nqueens(4)

// O(2^n) iterative
function powerset(arr) {
  const ps = [[]]
  for (const el of arr) {
    for (let i = 0, len = ps.length; i < len; ++i) {
      ps.push([...ps[i], el])
    }
  }
  return ps
}

// O(2^n) tail-recursive
function powerset2(arr, ps = [[]]) {
  if (arr.length === 0) { return ps }
  for (let i = 0, len = ps.length; i < len; ++i) {
    ps.push([...ps[i], arr[0]])
  }
  return powerset2(arr.slice(1), ps)
}

// O(2^n)
function powerset3(arr) {
  if (arr.length === 0) { return [[]] }
  const ss = powerset3(arr.slice(1))
  return [...ss, ...ss.map(s => [arr[0], ...s])]
}

// [[], [1], [1, 2], [1, 2, 3]].forEach(arr => console.log(powerset3(arr)))

// O(n!) dynamic multiple recursion
function permutations(arr, pp = [], ps = []) {
  if (arr.length === 0) { ps.push(pp); return ps }
  for (const el of arr) {
    permutations(arr.filter(e => e !== el), [...pp, el], ps)
  }
  return ps
}

// O(n!) dynamic multiple recursion
function permutations2(arr) {
  if (arr.length === 0) { return [[]] }
  return arr.flatMap(el =>
    permutations2(arr.filter(e => e !== el)).map(p => [el, ...p]))
}

// [[], [1], [1, 2], [1, 2, 3]].forEach(arr => console.log(permutations2(arr)))

// Backtracking
function subsetSum(arr, x, ps = [[]], ss = []) {
  if (arr.length === 0) { return ss }
  for (let i = 0, len = ps.length; i < len; ++i) {
    const cnd = [...ps[i], arr[0]]
    const sum = cnd.reduce((sum, el) => sum + el, 0)
    if (x === sum) { ss.push(cnd) }
    if (sum < x) { ps.push(cnd) }
  }
  return subsetSum(arr.slice(1), x, ps, ss)
}

// [[[1, 2, 3], 5], [[2, 6, 3, 5], 8], [[1, 2, 3, 5, 6, 7, 9], 13]]
//   .forEach(([arr, x]) => console.log(subsetSum(arr, x)))
