function swap(arr, i, j) {
  [arr[i], arr[j]] = [arr[j], arr[i]]
}

function matrix(m, n = m, v) {
  return Array(m).fill().map(_ => Array(n).fill(v))
}

// O(m*n)
function latticeMul(x, y) {
  x.reverse(); y.reverse()
  let prd = 0
  for (let i = 0; i < x.length; ++i) {
    for (let j = 0; j < y.length; ++j) {
      prd += x[i] * y[j] * 10 ** (i + j)
    }
  }
  return prd
}

// console.log(latticeMul([1, 2, 3], [2, 3, 4]))

// O(m*n)
function fibonacciMul(x, y) {
  x.reverse(); y.reverse()
  const m = x.length, n = y.length
  const prd = []
  let dgt = 0
  for (let k = 0; k < m + n - 1; ++k) {
    for (let i = 0; i < m; ++i) {
      for (let j = 0; j < n; ++j) {
        if (i + j === k) { dgt += x[i] * y[j] }
      }
    }
    prd.push(dgt % 10)
    dgt = Math.floor(dgt / 10)
  }
  return prd
}

// console.log(fibonacciMul([1, 2, 3], [2, 3, 4]))

// O(m*n)
function schoolMul(x, y) {
  let prd = 0, i = 0
  while (y > 0) {
    prd += x * (y % 10) * 10 ** i++
    y = Math.floor(y / 10)
  }
  return prd
}

// console.log(schoolMul(123, 234))

// O(m*n)
function peasantMul(x, y) {
  if (x > y) { [x, y] = [y, x] }
  let prd = 0
  while (x > 0) {
    if (x % 2 === 1) { prd += y }
    x = Math.floor(x / 2) // mediation
    y += y // duplation
  }
  return prd
}

// console.log(peasantMul(123, 234))

// O(m*n)
function peasantMul2(x, y) {
  if (x === 0) { return 0 }
  if (x > y) { [x, y] = [y, x] }
  const mx = Math.floor(x / 2), dy = y + y
  if (x % 2 === 1) { return y + peasantMul2(mx, dy) }
  return peasantMul2(mx, dy)
}

// console.log(peasantMul2(123, 234))

// O(2^n)
function towerOfHanoi(n, o = "Org", d = "Dst", a = "Aux") {
  if (n > 0) {
    towerOfHanoi(n - 1, o, a, d)
    console.log(`${n}: ${o} => ${d}`)
    towerOfHanoi(n - 1, a, d, o)
  }
}

// towerOfHanoi(3)

// O(n)
function merge(a, b) {
  const m = a.length, n = b.length
  let i = 0, j = 0, arr = []
  while (i < m && j < n) {
    a[i] < b[j] ? arr.push(a[i++]) : arr.push(b[j++])
  }
  while (i < m) { arr.push(a[i++]) }
  while (j < n) { arr.push(b[j++]) }
  return arr
}

// O(n*log(n))
function mergesort(arr) {
  if (arr.length < 2) { return arr }
  const m = Math.floor(arr.length / 2)
  return merge(mergesort(arr.slice(0, m)), mergesort(arr.slice(m)))
}

// [[], [1], [1, 2], [2, 1], [8, 0, 1, 4, 2, 3, 6, 7, 5, 9, 0]
// ].forEach(arr => console.log(arr, mergesort(arr)))

// O(n)
function hoarePartition(arr, l = 0, r = arr.length - 1) {
  if (l === r) { return l }
  if (l < r) {
    const p = l++
    while (true) {
      while (arr[l] < arr[p]) { ++l }
      while (arr[p] < arr[r]) { --r }
      if (l < r) { swap(arr, l, r); ++l; --r }
      else { swap(arr, p, r); return r }
    }
  }
}

// O(n)
function lomutoPartition(arr, l = 0, r = arr.length - 1) {
  if (l === r) { return l }
  if (l < r) {
    let j = l // r is the pivot
    for (let i = l; i < r; ++i) {
      if (arr[i] < arr[r]) { swap(arr, j++, i) }
    }
    swap(arr, j, r)
    return j
  }
}

// O(n*log(n))
function quicksort(arr, l = 0, r = arr.length - 1) {
  if (l < r) {
    let i = l + 1, j = r // l is the pivot
    while (true) {
      while(arr[i] < arr[l]) { ++i }
      while(arr[l] < arr[j]) { --j }
      if (i < j) { swap(arr, i, j); ++i; --j }
      else { swap(arr, l, j); break }
    }
    quicksort(arr, l, j - 1); quicksort(arr, j + 1, r)
  }
}

function quicksort2(arr, l = 0, r = arr.length - 1) {
  if (l < r) {
    // const p = hoarePartition(arr, l, r)
    const p = lomutoPartition(arr, l, r)
    quicksort2(arr, l, p - 1); quicksort2(arr, p + 1, r)
  }
}


// [[], [1], [1, 2], [2, 1], [8, 0, 1, 4, 2, 3, 6, 7, 5, 9, 0]
// ].forEach(arr => { console.log(arr); quicksort2(arr); console.log(arr) })

// O(n)
function quickselect(arr, k, l = 0, r = arr.length - 1) {
  if (l > r || k > r) { return }
  if (l === r) { return arr[l] }
  // const p = hoarePartition(arr, l, r)
  const p = lomutoPartition(arr, l, r)
  if (p === k) { return arr[p] }
  if (p < k) { return quickselect(arr, k, p + 1, r) }
  return quickselect(arr, k, l, p - 1)
}

// [[], [1], [1, 2], [2, 1], [8, 0, 1, 4, 2, 3, 6, 7, 5, 9]
// ].forEach(arr => { console.log(arr, quickselect(arr, 0)) })

// O(log(n))
function power(a, n) {
  if (n === 0) { return 1 }
  if (n % 2 === 1) { return a * power(a, Math.floor(n / 2)) ** 2 }
  return power(a, n / 2) ** 2
}

// console.log(power(2, 2), power(2, 3))

function nqueens(n, qrow = 0, brd = Array(n)) {
  if (qrow === n) { console.log(brd) }
  else {
    for (let col = 0; col < n; ++col) {
      let free = true
      for (let i = 0; i < qrow; ++i) {
        if (brd[i] === col ||
            brd[i] === col + qrow - i ||
            brd[i] === col - qrow + i
           ) { free = false; break }
      }
      if (free) {
        brd[qrow] = col
        nqueens(n, qrow + 1, brd)
      }
    }
  }
}

// nqueens(4)

// O(2^n) multiple recursion
function subsetSum(arr, t) {
  if (t === 0) { return true }
  if (arr.length === 0) { return false }
  for (const el of arr) {
    const exa = arr.filter(e => e !== el)
    return subsetSum(exa, t - el) || subsetSum(exa, t)
  }
}

// O(2^n) multiple recursion
function subsetSum2(arr, t, i = arr.length - 1) {
  if (t === 0) { return true }
  if (i === -1) { return false }
  return subsetSum2(arr, t - arr[i], i - 1) ||
    subsetSum2(arr, t, i - 1)
}

// [[[1, 2, 3, 8], 5], [[8, 7, 6, 5, 3, 10, 9], 15], [[11, 6, 5, 1, 7, 13, 12], 15]
// ].forEach(([arr, t]) => console.log(arr, t, subsetSum(arr, t)))

// lis = longest increasing subsequence
// O(2^n) multiple recursion
function lis(arr) {
  function lis(i, j) {
    if (j > arr.length - 1) { return 0 }
    if (arr[j] < arr[i]) { return lis(i, j + 1) }
    const wi = lis(j, j + 1) + 1
    const wo = lis(i, j + 1)
    return Math.max(wi, wo)
  }
  arr.unshift(-Infinity)
  return lis(0, 1)
}

// O(n^2) dynamic programming
function lis2(arr) {
  arr.unshift(-Infinity)
  const n = arr.length, len = matrix(n)
  for (let i = 0; i < n; ++i) { len[i][n - 1] = 0 }
  for (let j = n - 2; j >= 0; --j) {
    for (let i = 0; i < j; ++i) {
      const wi = len[j][j + 1] + 1
      const wo = len[i][j + 1]
      len[i][j] = arr[j] < arr[i] ? wo : Math.max(wi, wo)
    }
  }
  return len[0][1]
}

// O(2^n) multiple recursion
function lis3(arr) {
  function lis3(i) {
    let len = 0
    for (let j = i + 1; j < arr.length; ++j) {
      if (arr[i] < arr[j]) {
        len = Math.max(len, lis3(j))
      }
    }
    return len + 1
  }
  arr.unshift(-Infinity)
  return lis3(0) - 1
}

// O(n^2) dynamic programming
function lis4(arr) {
  arr.unshift(-Infinity)
  const n = arr.length, len = Array(n)
  for (let i = n - 1; i >= 0; --i) {
    len[i] = 1
    for (let j = i + 1; j < n; ++j) {
      if (arr[i] < arr[j]) {
        len[i] = Math.max(len[i], 1 + len[j])
      }
    }
  }
  return len[0] - 1
}

// [[6, 1, 5, 3, 8, 9, 4]].forEach(arr => console.log(arr, lis4(arr)))

// O(2^n) multiple recursion
function fib(n) {
  if (n < 2) { return n }
  return fib(n - 1) + fib(n - 2)
}

// O(n) memoization, top-down
function fib2(n, cache = Array(n)) {
  if (n < 2) { return n }
  if (cache[n]) { return cache[n] }
  return cache[n] = fib2(n - 1, cache) + fib2(n - 2, cache)
}

// O(n) dynamic programming, bottom-up, array
function fib3(n, cache = [0, 1]) {
  for (let i = 2; i <= n; ++i) {
    cache[i] = cache[i - 1] + cache[i - 2]
  }
  return cache[n]
}

// O(n) dynamic programming, bottom-up, variables
function fib4(n) {
  let a = - 1, b = 1, c = a + b
  while (n-- >= 0) { c = a + b; a = b; b = c }
  return c
}

// for (const i of Array(10).keys()) { console.log(fib4(i)) }

// O(m*n) dynamic programming
function editDistance(a, b) {
  const m = a.length + 1, n = b.length + 1, dist = matrix(m, n)
  for (let j = 0; j < n; ++j) { dist[0][j] = j }
  for (let i = 1; i < m; ++i) {
    dist[i][0] = i
    for (let j = 1; j < n; ++j) {
      const ins = dist[i][j - 1] + 1
      const del = dist[i - 1][j] + 1
      const sub = dist[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1)
      dist[i][j] = Math.min(ins, del, sub)
    }
  }
  return dist[m - 1][n - 1]
}

// [["food", "money"], ["algorithm", "altruistic"]
// ].forEach(([a, b]) => console.log(a, b, editDistance(a, b)))
