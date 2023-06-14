import { error, matrix } from "./util.js"

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
  const prd = matrix(am, bn)
  for (let i = 0; i < am; ++i) {
    for (let j = 0; j < bn; ++j) {
      let sum = 0
      console.log()
      for (let k = 0; k < an; ++k) {
        console.log(a[i][k], b[k][j])
        sum += a[i][k] * b[k][j]
      }
      prd[i][j] = sum
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
