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

export function pascalTriangle2(n) {
  function pt(i, j) {
    return j === 0 || i === j ? 1 :
      pt(i - 1, j - 1) + pt(i - 1, j)
  }
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

const n = 5
console.log(pascalTriangle(n))
console.log(pascalTriangle2(n))

