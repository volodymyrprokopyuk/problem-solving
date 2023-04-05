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
