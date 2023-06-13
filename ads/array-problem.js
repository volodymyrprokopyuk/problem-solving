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

// O(m*n) adds two compatible matrices
export function mxAdd(a, b) {
  const m = a.length, n = a[0]?.length
  if (!n || m !== b.length || n !== b[0]?.length) {
    error(`incompatible matrices`)
  }
  const sum = matrix(m, n)
  for (let i = 0; i < m; ++i) {
    for (let j = 0; j < n; ++j) {
      sum[i][j] = a[i][j] + b[i][j]
    }
  }
  return sum
}

// const a = matrix(2, 3, 10), b = matrix(2, 3, 20)
// // const a = matrix(1, 1, 2), b = matrix(1, 1, 2)
// console.log(mxAdd(a, b))
