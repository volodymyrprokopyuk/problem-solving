const eq = (a, b) => a === b
const lt = (a, b) => a < b
const le = (a, b) => a <= b
const gt = (a, b) => a > b
const ge = (a, b) => a >= b

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n)
function sumFirstN(n) {
  return n === 0 ? 0 : sumFirstN(n - 1) + n
}

// O(n) tail-recursive
function sumFirstN2(n, sum = 0) {
  return n === 0 ? sum : sumFirstN2(n - 1, sum + n)
}

// for (const i of Array(6).keys()) {
//   console.log(sumFirstN2(i))
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

// [0, 1, 2, 3, 4].forEach(n => console.log(power(2, n)))

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
    n % 10 === d || containsDigit(Math.floor(n / 10), d)
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
    a[0] === b[0] && equalString(a.slice(1), b.slice(1))
}

// O(n) tail-recursive
function equalString2(a, b) {
  return a.length !== b.length ? false :
    a === "" ? true :
    a[0] !== b[0] ? false :
    equalString2(a.slice(1), b.slice(1))
}

// [["", ""], ["", "a"], ["a", "b"], ["abc", "abc"]].forEach(([a, b]) =>
//   console.log(equalString(a, b))
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
function partitionFilter(arr, p, cmp) {
  if (arr.length === 0) { return [] }
  else if (cmp(arr[0], p)) {
    return [arr[0], ...partitionFilter(arr.slice(1), p, cmp)]
  } else { return partitionFilter(arr.slice(1), p, cmp) }
}

// O(n)
function basicPartion(arr, p) {
  return [
    ...partitionFilter(arr, p, lt),
    ...partitionFilter(arr, p, eq),
    ...partitionFilter(arr, p, gt)
  ]
}

// [[4], [1, 4], [7, 2, 1, 4, 3, 6, 4, 8, 5, 9, 0]].forEach(arr =>
//   console.log(basicPartion(arr, 4))
// )

// O(n)
function hoarePartition(
  arr, l = 0, r = arr.length - 1,
  p = arr[Math.floor((l + r) / 2)]
) {
  while (true) {
    while (arr[l] < p) { ++l }
    while (p < arr[r]) { --r }
    if (l < r) { swap(arr, l++, r--) }
    else { return l }
  }
}

// O(n) tail-recursive
function hoarePartition2(
  arr, l = 0, r = arr.length - 1,
  p = arr[Math.floor((l + r) / 2)]
) {
  if (r < l) { return l - 1 }
  else if (p < arr[l] && arr[r] < p) {
    swap(arr, l, r)
    return hoarePartition2(arr, ++l, --r, p)
  } else {
    if (arr[l] <= p) { ++l }
    if (p < arr[r]) { --r }
    return hoarePartition2(arr, l, r, p)
  }
}

// [[], [1], [1, 2], [1, 2, 3],
//  [7, 2, 1, 4, 3, 6, 4, 8, 5, 9, 0],
//  [2, 1, 3, 0, 4, 9, 5, 6, 10, 7, 8]
// ].forEach(arr =>
//   console.log(arr, hoarePartition(arr))
// )

// O(n) tail-recursive
function quickSelect(arr, k, l = 0, r = arr.length - 1) {
  if (l === r) { return arr[l] }
  const i = hoarePartition(arr, l, r)
  if (i === k) { return arr[i] }
  if (i < k) { return quickSelect(arr, k, i + 1, r) }
  else { return quickSelect(arr, k, l, r - 1) }
}

// [[7, 2, 1, 4, 3, 6, 4, 8, 5, 9, 0],
//  [2, 1, 3, 0, 4, 9, 5, 6, 10, 7, 8]
// ].forEach(arr =>
//   console.log(arr, quickSelect(arr, 2))
// )
