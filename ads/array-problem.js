import { error, swap, matrix } from "./util.js"
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

const mx = [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]]
console.log(mx)
console.log(mxDiags(mx))

// O(n) exchanges the k-th biggest/smallest elements in an array
export function exKthBigSmall(arr, k = 0) {
  const n = arr.length, a = arr.slice()
  if (k >= n) { return }
  const sm = a[quickSelect(a, k)], bg = a[quickSelect(a, n - k - 1)]
  console.log(sm, bg)
  let i = -1, j = -1
  for (let k = 0; k < n; ++k) {
    if (i === -1 && arr[k] === sm) { i = k }
    if (j === -1 && arr[k] === bg) { j = k }
    if (i !== -1 && j !== -1) { break }
  }
  console.log(i, j)
  if (i !== -1 && j !== -1 && i !== j) { swap(arr, i, j) }
  return arr
}
