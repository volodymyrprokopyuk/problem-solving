function swap(arr, i, j) { [arr[i], arr[j]] = [arr[j], arr[i]] }

function matrix(m, n = m, v) {
  return Array(m).fill().map(_ => Array(n).fill(v))
}

// recursive
function factorial(n) {
  return n === 0 ? 1 : n * factorial(n - 1)
}

// iterative
function factorial2(n) {
  let fact = 1
  for (let i = 1; i <= n; ++i) { fact *= i }
  return fact
}

// [0, 1, 2, 3, 4].forEach(n => console.log(factorial2(n)))

function currentSum(arr, i = arr.length - 1) {
  return i === 0 ? arr[i] : arr[i] += currentSum(arr, i - 1)
}

// let arr = [0, 1, 2, 3, 4, 5, 6]; currentSum(arr); console.log(arr)

// iteartive
function bubbleSort(arr) {
  for (let i = 0; i < arr.length - 1; ++i) {
    for (let j = 0; j < arr.length - i; ++j) {
      if (arr[j + 1] < arr[j]) { swap(arr, j, j + 1) }
    }
  }
}

// recursive
function bubbleSort2(arr, n = arr.length - 1) {
  if (n <= 0) { return }
  for (let i = 0; i < n; ++i) {
    if (arr[i + 1] < arr[i]) { swap(arr, i, i + 1) }
  }
  bubbleSort(arr, n - 1)
}

// [[], [1], [1, 2], [2, 1], [0, 3, 1, 8, 5, 6, 2, 7, 4, 9]]
//   .forEach(arr => { bubbleSort2(arr); console.log(arr) })

function mulTable(n = 9, i = n, j = 9) {
  if (j < 1) { return }
  if (i < 1) {
    mulTable(n, n, j - 1)
    process.stdout.write("\n")
  } else {
    mulTable(n, i - 1, j)
    process.stdout.write(`${i} x ${j} = ${i * j}  `)
  }
}

// mulTable()

// O(2^n) time, O(2^n) memory, overlapping subproblems, top-down
function minCostTravel(s, d, tk) {
  if (s === d || s === d - 1) { return tk[s][d] }
  let minCost = tk[s][d]
  for (let i = s + 1; i < d; ++i) {
    const newCost = minCostTravel(s, i, tk) +
          minCostTravel(i, d, tk)
    if (newCost < minCost) { minCost = newCost }
  }
  return minCost
}

// O(n^3) time, O(n^2) memory, momoization, top-down
function minCostTravel2(s, d, tk, cache = matrix(Math.max(s, d) + 1)) {
  if (s === d || s === d - 1) { return tk[s][d] }
  if (cache[s][d]) { return cache[s][d] }
  let minCost = tk[s][d]
  for (let i = s + 1; i < d; ++i) {
    const newCost = minCostTravel2(s, i, tk, cache) +
          minCostTravel2(i, d, tk, cache)
    if (newCost < minCost) { minCost = newCost }
  }
  cache[s][d] = minCost
  return cache[s][d]
}

// O(n^2) time, O(n) memory, dynamic programming, bottom-up
function minCostTravel3(s, d, tk, cache = Array(Math.max(s, d) + 1)) {
  cache[0] = tk[s][s] // cache[i] = min cost from s to i
  cache[1] = tk[s][s + 1]
  for (let i = s + 1; i <= d; ++i) {
    cache[i] = tk[s][i]
    for (let j = 1; j < i; ++j) {
      let newCost = cache[j] + tk[j][i]
      if (newCost < cache[i]) { cache[i] = newCost }
    }
  }
  return cache[d]
}

// const travelTickets = [
//   [0, 10, 75, 94],
//   [-1, 0, 35, 50],
//   [-1, -1, 0, 80],
//   [-1, -1, -1, 0]
// ]
// console.log(minCostTravel3(0, 3, travelTickets))

// O(n^3)
function maxSubstrLength(str) {
  function sum(str) {
    return str.split("").reduce((sm, el) => sm + parseInt(el), 0)
  }
  let maxLen = 0, ml = "", mr = ""
  for (let i = 0; i < str.length; ++i) {
    for (let j = i + 1; j < str.length; j += 2) {
      const m = Math.ceil((i + j) / 2), len = j - i + 1
      if (len > maxLen) {
        const l = str.slice(i, m), r = str.slice(m, j + 1)
        if (sum(l) === sum(r)) { maxLen = len; ml = l; mr = r }
      }
    }
  }
  return [maxLen, ml, mr]
}

// O(n^2) dynamic programming
function maxSubstrLength2(str) {
  const sum = matrix(str.length)
  for (let i = 0; i < str.length; ++i) {
    sum[i][i] = parseInt(str[i])
  }
  for (let i = 0; i < str.length; ++i) {
    for (let j = i + 1; j < str.length; ++j) {
      sum[i][j] = sum[i][j - 1] + parseInt(str[j])
    }
  }
  let maxLen = 0, ml = "", mr = ""
  for (let i = 0; i < str.length; ++i) {
    for (let j = i + 1; j < str.length; j += 2) {
      let m = Math.floor((i + j) / 2), len = j - i + 1
      if (len > maxLen && sum[i][m] === sum[m + 1][j]) {
        maxLen = len
        ml = str.slice(i, m + 1)
        mr = str.slice(m + 1, j + 1)
      }
    }
  }
  return [maxLen, ml, mr]
}

// ["142124", "9430723"].forEach(str => console.log(str, maxSubstrLength2(str)))
