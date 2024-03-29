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

class TNode {
  key; data
  left = null; right = null

  constructor(key, data = key) {
    this.key = key; this.data = data
  }
}

function postOrder(nd, post) {
  if (!nd) { return }
  const l = postOrder(nd.left, post)
  const r = postOrder(nd.right, post)
  return post(nd, l, r)
}

// const tn2 = new TNode(2)
// const tn4 = new TNode(4)
// const tn1 = new TNode(1)
// const tn2a = new TNode(2)
// const tn6 = new TNode(6)
// const tn9 = new TNode(9)
// const tn3 = new TNode(3)
// tn2.left = tn4; tn2.right = tn1
// tn4.left = tn6; tn4.right = tn9
// tn9.left = tn3
// tn1.right = tn2a
// postOrder(tn2, (nd, l = 0, r = 0) => nd.key += l + r)
// postOrder(tn2, nd => console.log(nd.key))

// O(2^n) recursive, O(n^2) memoization
function minCostPath(x, y, cost, cache = matrix(cost.length, cost[0].length)) {
  if (cache[x][y]) { return cache[x][y] }
  if (x === 0 && y === 0) {
    return cache[x][y] = cost[x][y]
  }
  if (x === 0) {
    return cache[x][y] = minCostPath(x, y - 1, cost, cache) + cost[0][y]
  }
  if (y === 0) {
    return cache[x][y] = minCostPath(x - 1, y, cost, cache) + cost[x][0]
  }
  const up = minCostPath(x - 1, y, cost, cache)
  const left = minCostPath(x, y - 1, cost, cache)
  return cache[x][y] = Math.min(up, left) + cost[x][y]
}

// O(n^2) dynamic programming
function minCostPath2(x, y, cost) {
  const m = cost.length, n = cost[0].length
  const minCost = matrix(m, n)
  minCost[0][0] = cost[0][0]
  for (let j = 1; j < n; ++j) {
    minCost[0][j] = minCost[0][j - 1] + cost[0][j]
  }
  for (let i = 1; i < m; ++i) {
    minCost[i][0] = minCost[i - 1][0] + cost[i][0]
  }
  for (let i = 1; i < m; ++i) {
    for (let j = 1; j < n; ++j) {
      minCost[i][j] =
        Math.min(minCost[i - 1][j], minCost[i][j - 1]) + cost[i][j]
    }
  }
  return minCost[x][y]
}

// const costMatrix = [[1, 3, 5, 8], [4, 2, 1, 7], [4, 3, 2, 3]]
// console.log(minCostPath2(2, 3, costMatrix))

// O(n) recursive + memoization
function tilePlacement(n, cache = Array(n)) {
  if (cache[n]) { return cache[n] }
  if (n < 3) { return n }
  return cache[n] = tilePlacement(n - 1, cache) + tilePlacement(n - 2, cache)
}


// O(n) dynamic programming
function tilePlacement2(n) {
  let a = 1, b = 2, c = a + b
  if (n === 1) { return a }
  if (n === 2) { return b }
  for (let i = 4; i <= n; ++i) {
    c = a + b; a = b; b = c
  }
  return c
}

// [1, 2, 3, 4, 5].forEach(n => console.log(tilePlacement(n)))

// O(3^n) recursive + memoization
function totalScore(n, cache = Array(n)) {
  if (cache[n]) { return cache[n] }
  if (n < 0) { return 0 }
  if (n === 0) { return 1 }
  return cache[n] = totalScore(n - 10, cache) +
    totalScore(n - 5, cache) +
    totalScore(n - 3, cache)
}

// O(n) dynamic programming
function totalScore2(n) {
  const ways = Array(n + 1).fill(0)
  ways[0] = 1
  for (let i = 1; i <= n; ++i) {
    if (i - 3 >= 0) { ways[i] += ways[i - 3] }
    if (i - 5 >= 0) { ways[i] += ways[i - 5] }
    if (i - 10 >= 0) { ways[i] += ways[i - 10] }
  }
  return ways[n]
}

// [2, 3, 7, 10, 13, 18, 24].forEach(n => console.log(totalScore(n)))

// O(n^2)
function maxSubArraySum(arr) {
  let maxSum = -Infinity, a = 0, b = 0
  for (let i = 0; i < arr.length; ++i) {
    let sum = arr[i]
    for (let j = i + 1; j < arr.length; ++j) {
      sum += arr[j]
      if (sum > maxSum) { maxSum = sum, a = i, b = j }
    }
  }
  return [maxSum, arr.slice(a, b + 1)]
}

// console.log(maxSubArraySum([-2, -3, 4, -1, -2, 1, 5, -3]))

// O(n^3) multiple recursion
function minEditDistance(a, b) {
  if (a.length === 0) { return b.length }
  if (b.length === 0) { return a.length }
  if (a[0] === b[0]) {
    return minEditDistance(a.slice(1), b.slice(1))
  }
  const ins = minEditDistance(a, b.slice(1))
  const rep = minEditDistance(a.slice(1), b.slice(1))
  const rem = minEditDistance(a.slice(1), b)
  return Math.min(ins, rep, rem) + 1
}

// O(n^2) time, O(n^2) space, dynamic programming
function minEditDistance2(a, b) {
  const m = a.length + 1, n = b.length + 1
  const dist = matrix(m, n)
  for (let i = 0; i < m; ++i) { dist[i][0] = i }
  for (let j = 0; j < n; ++j) { dist[0][j] = j }
  for (let i = 1; i < m; ++i) {
    for (let j = 1; j < n; ++j) {
      if (a[i - 1] === b[j - 1]) { dist[i][j] = dist[i - 1][j - 1] }
      else {
        dist[i][j] =
          Math.min(dist[i - 1][j], dist[i - 1][j - 1], dist[i][j - 1]) + 1
      }
    }
  }
  return dist[m - 1][n - 1]
}

// [["cat", "car"], ["sunday", "saturday"], ["abc", "xyz"], ["food", "money"]
// ].forEach(([a, b]) => console.log(a, b, minEditDistance2(a, b)))

// O(2^n) multiple recursion
function totalPathCount(m, n) {
  if (m === 1 || n === 1) { return 1 }
  return totalPathCount(m - 1, n) + totalPathCount(m, n - 1)
}

// O(n^2) dynamic programming
function totalPathCount2(m, n) {
  const path = matrix(m, n)
  for (let j = 0; j < n; ++j) { path[0][j] = 1 }
  for (let i = 0; i < m; ++i) { path[i][0] = 1 }
  for (let i = 1; i < m; ++i) {
    for (let j = 1; j < n; ++j) {
      path[i][j] = path[i - 1][j] + path[i][j - 1]
    }
  }
  return path[m - 1][n - 1]
}

// [[1, 1], [2, 2], [3, 4]
// ].forEach(([m, n]) => console.log(totalPathCount2(m, n)))

// O(2^n) multiple recursion
function isStrInterleaved(a, b, c) {
  const al = a.length, bl = b.length, cl = c.length
  if (al === 0 && bl === 0 && cl === 0) { return true }
  if (al === 0 && bl === 0 && cl !== 0) { return false }
  if ((al !== 0 || bl !== 0) && cl === 0) { return false }
  return a[0] === c[0] && isStrInterleaved(a.slice(1), b, c.slice(1)) ||
    b[0] === c[0] && isStrInterleaved(a, b.slice(1), c.slice(1))
}

// O(n^2) dynamic programming
function isStrInterleaved2(a, b, c) {
  const m = a.length, n = b.length, cl = c.length
  if (m + n !== cl) { return false }
  const ilvd = matrix(m + 1, n + 1)
  ilvd[0][0] = true
  for (let j = 1; j <= n; ++j) {
    if (b[j - 1] !== c[j - 1]) { ilvd[0][j] = false }
    else { ilvd[0][j] = ilvd[0][j - 1] }
  }
  for (let i = 1; i <= m; ++i) {
    if (a[i - 1] !== c[i - 1]) { ilvd[i][0] = false }
    else { ilvd[i][0] = ilvd[i - 1][0] }
  }
  for (let i = 1; i <= m; ++i) {
    for (let j = 1; j <= n; ++j) {
      let ac = a[i - 1], bc = b[j - 1], cc = c[i + j - 1]
      ilvd[i][j] =
        ac === cc && bc === cc ? ilvd[i - 1][j] || ilvd[i][j - 1] :
        ac === cc && bc !== cc ? ilvd[i - 1][j] :
        ac !== cc && bc === cc ? ilvd[i][j - 1] : false
    }
  }
  return ilvd[m][n]
}

// [
//   ["xyz", "abcd", "xabyczd"], ["ab", "xy", "abxyZ"], ["ab", "xy", "abx"],
//   ["ab", "ac", "abac"], ["bcc", "bbca", "bbcbcac"]
// ].forEach(([a, b, c]) => console.log(a, b, c, isStrInterleaved2(a, b, c)))

// O(2^n) multiple recursion
function subsetSum(arr, x) {
  if (x === 0) { return true }
  if (arr.length === 0) { return false }
  if (arr[0] > x) { return subsetSum(arr.slice(1), x) }
  return subsetSum(arr.slice(1), x - arr[0]) ||
    subsetSum(arr.slice(1), x)
}

// [[[3, 2, 7, 1], 6], [[1, 2, 5], 4]
// ].forEach(([arr, x]) => console.log(arr, x, subsetSum(arr, x)))

// lcs = longest common subsequence
// O(2^n) multiple recursion
function lcs(a, b) {
  if (a.length === 0 || b.length === 0) { return 0 }
  if (a[0] === b[0]) {
    return 1 + lcs(a.slice(1), b.slice(1))
  } else {
    return Math.max(lcs(a.slice(1), b), lcs(a, b.slice(1)))
  }
}

// O(2^n) multiple recursion
function lcs2(a, b) {
  if (a.length === 0 || b.length === 0) { return [0, ""] }
  if (a[0] === b[0]) {
    const [len, lcs] = lcs2(a.slice(1), b.slice(1))
    return [1 + len, a[0] + lcs]
  } else {
    const [lena, lcsa] = lcs2(a.slice(1), b)
    const [lenb, lcsb] = lcs2(a, b.slice(1))
    return lena > lenb ? [lena, lcsa] : [lenb, lcsb]
  }
}

// O(n^2) memmoization
function lcs3(a, b, cache = matrix(a.length + 1, b.length + 1)) {
  const m = a.length, n = b.length
  if (m === 0 || n === 0) { return 0 }
  if (cache[m][n]) { return cache[m][n] }
  if (a[0] === b[0]) {
    return cache[m][n] = 1 + lcs2(a.slice(1), b.slice(1), cache)
  }
  return cache[m][n] =
    Math.max(lcs2(a.slice(1), b, cache), lcs2(a, b.slice(1), cache))
}

// O(n^2) dynamic programming
function lcs4(a, b) {
  const m = a.length, n = b.length
  const ssl = matrix(m + 1, n + 1)
  let lcs = ""
  for (let j = 0; j <= n; ++j) { ssl[0][j] = 0 }
  for (let i = 0; i <= m; ++i) { ssl[i][0] = 0 }
  for (let i = 1; i <= m; ++i) {
    for (let j = 1; j <= n; ++j) {
      ssl[i][j] = a[i - 1] === b[j - 1] ?
        (lcs += a[i - 1], ssl[i][j] = 1 + ssl[i - 1][j - 1]) :
        Math.max(ssl[i - 1][j], ssl[i][j - 1])
    }
  }
  return [ssl[m][n], lcs]
}

// [["abcd", "aebd"], ["abcde", "apqbr"]
// ].forEach(([a, b]) => console.log(a, b, lcs2(a, b)))

// greedy algorithm does not work for all cases e. g. 65
function coinChange(x, cs = [1, 2, 5, 10, 12, 20, 50]) {
  if (x === 0) { return [] }
  for (let i = cs.length - 1; i >= 0; --i) {
    const coin = cs[i]
    if (x >= coin) {
      const cnum = [coin, Math.floor(x / coin)]
      return [cnum, ...coinChange(x % coin, cs)]
    }
  }
}

// O(m^n) multiple recursion
function coinChange2(x, cs = [1, 2, 5, 10, 12, 20, 50]) {
  function cCount(cs) { return cs.reduce((s, [_, n]) => s + n, 0) }
  if (x === 0) { return [] }
  let minCs = [[undefined, Infinity]]
  for (let i = 0; i < cs.length; ++i) {
    const coin = cs[i]
    if (x >= coin) {
      const tmpCs = coinChange2(x % coin, cs)
      if (cCount(tmpCs) + 1 < cCount(minCs)) {
        const cnum = [coin, Math.floor(x / coin)]
        minCs = [cnum, ...tmpCs]
      }
    }
  }
  return minCs
}

// O(n^2) dynamic programming
function coinChange3(x, cs = [1, 2, 5, 10, 12, 20, 50]) {
  const minCs = Array(x + 1).fill(Infinity)
  minCs[0] = 0
  for (let i = 0; i <= x; ++i) {
    for (let j = 0; j < cs.length; ++j) {
      const coin = cs[j]
      if (i >= coin) {
        const mcs = minCs[i - coin]
        if (mcs !== Infinity && mcs + 1 < minCs[i]) {
          minCs[i] = mcs + 1
        }
      }
    }
  }
  return minCs[x]
}

// [1, 2, 3, 4, 9, 11, 27, 65].forEach(x => console.log(x, coinChange3(x)))

// O(n^m) multiple recursion + memoization
function cutRodMaxPrice(n, prices, cache = Array(n + 1)) {
  if (n === 0) { return prices[n] }
  if (cache[n]) { return cache[n] }
  let maxPrice = -Infinity
  for (let i = 1; i <= n; ++i) {
    cache[n] = maxPrice =
      Math.max(maxPrice, prices[i] + cutRodMaxPrice(n - i, prices, cache))
  }
  return cache[n]
}

// O(n^2) dynamic programming
function cutRodMaxPrice2(n, prices) {
  const maxPs =Array(n + 1).fill(-Infinity)
  maxPs[0] = 0
  for (let i = 1; i <= n; ++i) {
    maxPs[i] = prices[i]
    for (let j = 0; j < i; ++j) {
      maxPs[i] = Math.max(maxPs[i], prices[j] + maxPs[i - j])
    }
  }
  return maxPs
}

// const prices = [0, 1, 5, 8, 9, 10, 17, 17, 20];
// [0, 1, 2, 3, 4, 5, 6, 7, 8
// ].forEach(n => console.log(n, cutRodMaxPrice2(n, prices)))

// O(2^n) multiple recursion
function knapsack(ws, vs, cap) {
  if (ws.length === 0) { return 0 }
  if (ws[0] <= cap) {
    const incV = vs[0] + knapsack(ws.slice(1), vs.slice(1), cap - ws[0])
    const excV = knapsack(ws.slice(1), vs.slice(1), cap)
    return Math.max(incV, excV)
  }
  return knapsack(ws.slice(1), vs.slice(1), cap)
}

// O(n*cap) dynamic programming
function knapsack2(ws, vs, cap) {
  const m = ws.length + 1, n = cap + 1
  const val = matrix(m, n)
  for (let j = 0; j < n; ++j) { val[0][j] = 0 }
  for (let i = 0; i < m; ++i) { val[i][0] = 0 }
  for (let i = 1; i < m; ++i) {
    for (let j = 1; j < n; ++j) {
      if (ws[i - 1] <= j) {
        val[i][j] = Math.max(
          vs[i - 1] + val[i - 1][j - ws[i - 1]], val[i - 1][j]
        )
      } else { val[i][j] = val[i - 1][j] }
    }
  }
  return val[m - 1][cap]
}

// const weights = [2, 3, 4, 5], values = [3, 4, 5, 6];
// [5].forEach(maxW => console.log(knapsack2(weights, values, maxW)))

// lps = longest palindromic subsequence
// O(2^n) multiple recursion
function lps(str) {
  if (str.length === 0 || str.length === 1) { return str }
  const fst = str.at(0), lst = str.at(-1)
  if (fst === lst) {
    return fst + lps(str.slice(1, -1)) + lst
  }
  const beg = lps(str.slice(0, -1))
  const end = lps(str.slice(1))
  return beg.length > end.length ? beg : end
}

// ["aab", "aXZab", "ZabWbcVbbUaXY"].forEach(str => console.log(str, lps2(str)))
