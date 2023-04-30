function swap(arr, i, j) { [arr[i], arr[j]] = [arr[j], arr[i]] }

// O(n)
function countDownAndUp(n) {
  if (n === 0) { return }
  console.log(`Before ${n}`)
  countDownAndUp(n - 1)
  console.log(`After ${n}`)
}

// countDownAndUp(4)

// O(n) iterative
function factorial(n) {
  const st = []
  while (n > 0) { st.push(n--) }
  let fact = 1
  while (st.length > 0) { fact *= st.pop() }
  return fact
}

// [0, 1, 2, 3, 4].forEach(n => console.log(n, factorial(n)))

// O(n * m) iterative
function findSubstring(needle, haystack) {
  let i = 0
  while (needle.length <= haystack.length - i) {
    if (haystack.slice(i, i + needle.length) === needle) { return i }
    ++i
  }
  return -1
}

// O(n * m) tail-recursive
function findSubstring2(needle, haystack, i = 0) {
  if (haystack.length - i < needle.length) { return -1 }
  return haystack.slice(i, i + needle.length) === needle ? i :
    findSubstring2(needle, haystack, i + 1)
}

// [["", ""], ["a", "a"], ["abc", "_abc"], ["x", "ab"]
// ].forEach(([needle, haystack]) =>
//     console.log(findSubstring2(needle, haystack))
// )

// O(log(n))
function exponent(x, n) {
  return n === 0 ? 1 :
    n % 2 === 0 ? exponent(x, n / 2) ** 2 :
    exponent(x, Math.floor(n / 2)) ** 2 * x
}

// O(log(n)) iterative
function exponent2(x, n) {
  const st = []
  while (n > 0) {
    if (n % 2 === 0) { st.push("square"); n /= 2  }
    else { st.push("multiply"); n -= 1 }
  }
  let exp = 1
  while (st.length > 0) {
    const op = st.pop()
    if (op === "square") { exp **= 2 }
    else if (op === "multiply") { exp *= x }
  }
  return exp
}

// [0, 1, 2, 3, 4].forEach(n => console.log(exponent2(2, n)))

function sumArray([h = 0, ...t]) {
  return t.length === 0 ? h : h + sumArray(t)
}

// [[], [1], [1, 2], [1, 2, 3]].forEach(arr => console.log(arr, sumArray(arr)))

function reverseString([h = "", ...t]) {
  return t.length === 0 ? h : reverseString(t) + h
}

// ["", "a", "ab", "abc"].forEach(str => console.log(reverseString(str)))

function isPalindrome(str) {
  return str.length < 2 ? true :
    str.at(0) !== str.at(-1) ? false :
    isPalindrome(str.slice(1, -1))
}

// ["", "a", "ab", "aba", "abba", "abcdcbz"].forEach(str =>
//   console.log(str, isPalindrome(str))
// )

// O(n) multiple recursion
function floodFill(drw, x, y, newColor = "_", oldColor = drw[x][y]) {
  if (0 <= x < drw.length &&
      0 <= y < drw[0].length &&
      drw[x][y] === oldColor) {
    drw[x][y] = newColor
    for (const [nx, ny] of
         [[x - 1, y], [x + 1, y],
          [x, y - 1], [x, y + 1]]) {
      floodFill(drw, nx, ny, newColor, oldColor)
    }
  }
}

// const drawing = `..########################...........
// ..#......................#...#####...
// ..#..........########....#####...#...
// ..#..........#......#............#...
// ..#..........########.........####...
// ..######......................#......
// .......#..#####.....###########......
// .......####...#######................`
//   .split("\n").map(line => line.split(""))
// floodFill(drawing, 3, 7)
// drawing.forEach(line => console.log(line.join("")))

class TNode {
  key; data
  left = null; right = null
  constructor(key, data) {
    this.key = key; this.data = data
  }
}

class BSTree {
  #root
  #length = 0

  get length() { return this.#length }

  get depth() {
    return (function dep(nd) {
      return nd ? 1 + Math.max(dep(nd.left), dep(nd.right)) : -1
    })(this.#root)
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

  set(key, data = `${key}`, nd = this.#root) {
    if (!this.#length) {
      this.#root = new TNode(key, data)
      ++this.#length; return
    }
    if (key < nd.key) {
      if (!nd.left) {
        nd.left = new TNode(key, data)
        ++this.#length; return
      }
      this.set(key, data, nd.left)
    } else {
      if (!nd.right) {
        nd.right = new TNode(key, data)
        ++this.#length; return
      }
      this.set(key, data, nd.right)
    }
  }

  get(key, nd = this.#root) {
    if (nd) {
      if (key === nd.key) { return nd }
      if (key < nd.key) { return this.get(key, nd.left) }
      return this.get(key, nd.right)
    }
  }
}

// [[], [1], [1, 2, 3, 4, 5], [9, 4, 5, 1, 7, 3, 2, 8, 6, 0]] .forEach(arr => {
//   const tr = new BSTree();
//   arr.forEach(key => tr.set(key))
//   console.log(tr.length, tr.depth)
//   for (const nd of tr.inOrder) { console.log(nd.key) }
//   [2, 5, 8, 0, 10].forEach(key => console.log(tr.get(key)?.data))
// })

// O(log(n)) tail-recursive
function binarySearch(arr, el, l = 0, r = arr.length - 1) {
  const m = Math.floor((l + r) / 2)
  return r < l ? -1 :
    el === arr[m] ? m :
    el < arr[m] ? binarySearch(arr, el, l, m - 1) :
    binarySearch(arr, el, m + 1, r)
}

// [[[], 9], [[1], 9], [[1, 2], 1], [[1, 2], 2], [[1, 2, 3], 2]
// ].forEach(([arr, el]) => console.log(arr, el, binarySearch(arr, el)))

// O(n*log(n)) multiple recursion
function quickSort(arr, l = 0, r = arr.length - 1) {
  if (l < r) {
    let p = l, a = l + 1, b = r
    while (true) {
      while (arr[a] <= arr[p]) { ++a }
      while (arr[p] < arr[b]) { --b }
      if (a < b) { swap(arr, a, b); ++a; --b }
      else { swap(arr, p, b); break }
    }
    quickSort(arr, l, b); quickSort(arr, b + 1, r)
  }
}

[[], [1], [1, 2], [2, 1], [5, 8, 3, 7, 1, 2, 0, 4, 6, 9]
].forEach(arr => { quickSort(arr); console.log(arr) })
