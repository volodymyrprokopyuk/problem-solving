const lt = (a, b) => a < b
const gt = (a, b) => a > b

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

function sumFirstN(n) {
  if (n === 1) { return n }
  else { return sumFirstN(n - 1) + n }
}

function sumFirstN2(n) {
  return n === 1 ? 1 : sumFirstN2(n - 1) + n
}

function sumFirstN3(n, sum = 0) {
  return n === 0 ? sum : sumFirstN3(n - 1, sum + n)
}

// for (const i of Array(5).keys()) {
//   console.log(sumFirstN3(i + 1))
// }

function* fibonacci(n) {
  let a = -1, b = 1
  while (n-- > 0) {
    const c = a + b
    a = b, b = c
    yield c
  }
}

// console.log(...fibonacci(10))

// o(n)
function power(x, n) {
  if (n === 0) { return 1 }
  else if (n > 0) { return power(x, n - 1) * x }
  else { return power(x, n + 1) / x }
}

// [-4, -3, -2, -1, 0, 1, 2, 3, 4].forEach(n => console.log(power(2, n)))

// o(log(n))
function power2(x, n) {
  if (n === 0) { return 1 }
  else if (n % 2 === 0) { return power2(x, n / 2) ** 2 }
  else { return power2(x, (n - 1) / 2) ** 2 * x }
}

// [0, 1, 2, 3, 4].forEach(n => console.log(power(2, n)))

// O(n)
function add(a, b) {
  if (a === 0) { return b }
  else if (b === 0) { return a }
  else { return add(a - 1, b) + 1 }
}

// [[0, 1], [2, 3]].forEach(([a, b]) => console.log(add(a, b)))

// O(n)
function toBase(x, base = 2) {
  if (x < base) { return x }
  else { return toBase(Math.floor(x / base), base) * 10 + x % base }
}

// [0, 1, 2, 9, 15].forEach(x => console.log(toBase(x)))
// [47, 142].forEach(n => console.log(toBase(n, 5)))

// O(n)
function reverseString(s) {
  if (s === "") { return "" }
  else { return reverseString(s.slice(1)) + s.at(0) }
}

// ["", "a", "abc"].forEach(s => console.log(reverseString(s)))

// O(n)
function isPalindrome(s) {
  if (s.length < 2) { return true }
  else { return s.at(0) === s.at(-1) && isPalindrome(s.slice(1, -1)) }
}

// ["", "a", "ab", "abba", "abc", "abcba"].forEach(s =>
//   console.log(isPalindrome(s))
// )

// O(n^2)
function selectionSort(arr, cmp = lt) {
  if (arr.length < 2) { return arr }
  else {
    const m = arr.reduce((m, _, i) => cmp(arr[i], arr[m]) ? i : m, 0)
    if (m !== 0) { swap(arr, 0, m) }
    return [arr[0], ...selectionSort(arr.slice(1), cmp)]
  }
}

// [[], [1], [2, 3], [6, 4, 0, 1, 5, 3, 2]].forEach(arr =>
//   console.log(selectionSort(arr, gt))
// )

// O(n)
function hornerPolynomial(arr, x) {
  if (arr.length === 1) { return arr[0] }
  else { return hornerPolynomial(arr.slice(1), x) * x + arr[0] }
}

// [[1], [1, 2], [1, 1, 2, 3]].forEach(arr =>
//   console.log(hornerPolynomial(arr, 2))
// )

// O(n)
function containsDigit(n, d) {
  if (n < 10) { return n === d }
  else { return n % 10 === d || containsDigit(Math.floor(n / 10), d) }
}

// O(n) tail-recursive
function containsDigit2(n, d) {
  if (n < 10) { return n === d }
  else if (n % 10 === d) { return true }
  else { return containsDigit2(Math.floor(n / 10), d) }
}

// [1, 12, 123].forEach(n => console.log(containsDigit2(n, 2)))

// O(n)
function equalString(a, b) {
  if (a.length !== b.length) { return false }
  else if (a === "") { return true }
  else { return a[0] === b[0] && equalString(a.slice(1), b.slice(1)) }
}

// O(n) tail-recursive
function equalString2(a, b) {
  if (a.length !== b.length) { return false }
  else if (a === "") { return true }
  else if (a[0] !== b[0]) { return false }
  else { return equalString2(a.slice(1), b.slice(1)) }
}

// [["", ""], ["", "a"], ["a", "b"], ["abc", "abc"]].forEach(ss =>
//   console.log(equalString2(...ss))
// )

// O(n) tail-recursive
function linearSearch(arr, x, i = 0) {
  if (i === arr.length) { return -1 }
  else if (arr[i] === x) { return i }
  else { return linearSearch(arr, x, ++i) }
}

// [[[], 1], [[1], 1], [[1, 2, 3], 2], [[1, 2], 3]].forEach(([arr, x]) =>
//   console.log(linearSearch(arr, x))
// )

// O(log(n)) tail-recursive
function binarySearch(arr, x, a = 0, b = arr.length - 1) {
  if (a > b) { return -1 }
  const m = a + Math.floor((b - a) / 2)
  if (arr[m] === x) { return m }
  else if (x < arr[m]) { return binarySearch(arr, x, a, m - 1) }
  else { return binarySearch(arr, x, m + 1, b) }
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
    if (!this.length) {
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
    if (!nd) { return }
    else if (nd.key === key) { return nd }
    else if (key < nd.key) { return this.get(key, nd.left) }
    else { return this.get(key, nd.right) }
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
