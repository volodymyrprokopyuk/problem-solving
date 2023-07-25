import { error, arrSwap, matrix } from "./util.js"
import { quickSelect } from "./array.js"

// O(n^2) builds the Pascal's triangle of n rows
export function pascalTriangle(n) {
  const tr = [[1]]
  for (let i = 1; i <= n; ++i) {
    const row = [1]
    for (let j = 1; j < i; ++j) {
      row.push(tr[i - 1][j - 1] + tr[i - 1][j])
    }
    row.push(1)
    tr.push(row)
  }
  return tr
}

// O(n^2) recursion + memoization
export function pascalTriangle2(n) {
  function pt(i, j) {
    return j === 0 || i === j ? 1 :
      ca[i][j] ? ca[i][j] :
      ca[i][j] = pt(i - 1, j - 1) + pt(i - 1, j)
  }
  const ca = matrix(n + 1)
  const tr = []
  for (let i = 0; i <= n; ++i) {
    const row = []
    for (let j = 0; j <= i; ++j) {
      row.push(pt(i, j))
    }
    tr.push(row)
  }
  return tr
}

// O(m*n) multiplies a matrix on a scalar k
export function mxKMul(k, a) {
  const m = a.length, n = a[0]?.length
  if (!n) { error("mxKMul: not a matrix") }
  const mx = matrix(m, n)
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < n; ++j) {
      mx[i][j] = k * a[i][j]
    }
  }
  return mx
}

// O(m*n) adds two compatible matrices
export function mxAdd(a, b) {
  const m = a.length, n = a[0]?.length
  if (!n || m !== b.length || n !== b[0]?.length) {
    error("mxAdd: incompatible matrices")
  }
  const sum = matrix(m, n)
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < n; ++j) {
      sum[i][j] = a[i][j] + b[i][j]
    }
  }
  return sum
}

// O(m*n^2) multiplies two compatible matrix
export function mxMul(a, b) {
  const am = a.length, an = a[0]?.length,
        bm = b.length, bn = b[0]?.length
  if (!an || !bn || an !== bm) {
    error("mxMul: incompatible matrices")
  }
  const prd = matrix(am, bn, 0)
  for (let i = 0; i < am; ++i) {
    for (let j = 0; j < bn; ++j) {
      for (let k = 0; k < an; ++k) {
        prd[i][j] += a[i][k] * b[k][j]
      }
    }
  }
  return prd
}

// O(m*n) transposes a matrix
export function mxTrans(a) {
  const m = a.length, n = a[0]?.length
  if (!n) { error("mxTrans: not a matrix") }
  const tr = matrix(n, m)
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < n; ++j) {
      tr[j][i] = a[i][j]
    }
  }
  return tr
}

// O(m*n) fills a matrix diagonal, upper and lower triangles
export function mxFillDiag(mx, up = 1, dg = 0, lo = -1) {
  const m = mx.length, n = mx[0]?.length
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < n; ++j) {
      mx[i][j] = i < j ? up : i > j ? lo : dg
    }
  }
  return mx
}

export function mxDiags(mx) {
  const m = mx.length, pri = [], sec = []
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < m; ++j) {
      if (i === j) { pri.push(mx[i][j]) }
      if (i + j === m - 1) { sec.push(mx[i][j]) }
    }
  }
  return [pri, sec]
}

// O(n) exchanges the k-th biggest/smallest elements in an array
export function exKthBigSmall(arr, k = 0) {
  const n = arr.length, a = arr.slice()
  if (k >= n) { return }
  const sm = a[quickSelect(a, k)], bg = a[quickSelect(a, n - k - 1)]
  let i = -1, j = -1
  for (let k = 0; k < n; ++k) {
    if (i === -1 && arr[k] === sm) { i = k }
    if (j === -1 && arr[k] === bg) { j = k }
    if (i !== -1 && j !== -1) { break }
  }
  if (i !== -1 && j !== -1 && i !== j) { arrSwap(arr, i, j) }
  return arr
}

// O(n) returns an array which i-th element is the product of all other elements
export function productAllOther(arr) {
  const n = arr.length
  let p = 1
  for (let el of arr) { p *= el }
  const prd = Array(n)
  for (let i = 0; i < n; ++i) { prd[i] = p / arr[i] }
  return prd
}

// O(n) returns products of all other elements without using the division
export function productAllOther2(arr) {
  const n = arr.length, pre = Array(n), suf = Array(n)
  pre[0] = 1; suf[n - 1] = 1
  for (let i = 0; i < n - 1; ++i) { pre[i + 1] = pre[i] * arr[i] }
  for (let i = n - 1; i > 0; --i) { suf[i - 1] = suf[i] * arr[i] }
  const prd = Array(n)
  for (let i = 0; i < n; ++i) { prd[i] = pre[i] * suf[i] }
  return prd
}

// O(n*log(n)) returns indices of the smallest window to be sorted in an
// unsorted array
export function smallestWindowToSort(arr) {
  const n = arr.length, sor = arr.toSorted((a, b) => a - b)
  let i, j
  for (i = 0; i < n; ++i) { if (arr[i] !== sor[i]) { break } }
  for (j = n - 1; j >= 0; --j) { if (arr[j] !== sor[j]) { break } }
  return [i, j]
}

// O(n) returns indices of the smalles window to be sorted in an unsorted array
export function smallestWindowToSort2(arr) {
  const n = arr.length
  let max = -Infinity, min = Infinity, l = 0, r = n - 1
  for (let i = 0; i < n; ++i) {
    if (arr[i] > max) { max = arr[i] }
    if (arr[i] < max) { r = i }
  }
  for (let i = n - 1; i >= 0; --i) {
    if (arr[i] < min) { min = arr[i] }
    if (arr[i] > min) { l = i }
  }
  return [l, r]
}

// O(n^2) computes a max sub-array sum
export function maxSubArraySum(arr) {
  const n = arr.length, sum = matrix(n, n + 1)
  let max = 0, l = -1, r = -1
  for (let i = 0; i < n; ++i) { sum[i][i] = 0 }
  for (let i = 0; i < n; ++i) {
    for (let j = i; j < n; ++j) {
      const s = sum[i][j + 1] = sum[i][j] + arr[j]
      if (s > max) { max = s; l = i; r = j }
    }
  }
  return [max, l, r]
}

// O(n) computes a max sub-array sum
export function maxSubArraySum2(arr) {
  const n = arr.length, sum = Array(n)
  sum[0] = arr[0]
  let max = 0, l = -1, r = -1
  for (let i = 1; i < n; ++i) {
    if (sum[i - 1] > 0) {
      sum[i] = sum[i - 1] + arr[i]
      r = i
    } else {
      sum[i] = arr[i]
      if (sum[i] > max) { l = r = i }
    }
    if (sum[i] > max) { max = sum[i] }
  }
  return [max, l, r]
}

// O(n) computes a max sub-array sum
export function maxSubArraySum3(arr) {
  const n = arr.length
  let max = 0, l = -1, r = -1, curMax = arr[0]
  for (let i = 1; i < n; ++i) {
    if (curMax > 0) {
      curMax += arr[i]; r = i
    } else {
      curMax = arr[i]
      if (curMax > max) { l = r = i }
    }
    if (curMax > max) { max = curMax }
  }
  return [max, l, r]
}

// O(n^2) returns an array with number of smaller elements to the right
export function smallerToTheRight(arr) {
  const n = arr.length, sm = Array(n).fill(0)
  for (let i = 0; i < n; ++i) {
    for (let j = i; j < n; ++j) {
      if (arr[j] < arr[i]) { ++sm[i] }
    }
  }
  return sm
}
