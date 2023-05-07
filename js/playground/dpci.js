function swap(arr, i, j) { [arr[i], arr[j]] = [arr[j], arr[i]] }

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

// O(2^n) overlapping subproblems
function minCostTravel(s, d, tk) {
  if (s === d || s === d - 1) { return tk[s][d] }
  let minCost = tk[s][d]
  for (let i = s + 1; i < d; ++i) {
    const newCost = minCostTravel(s, i, tk) + minCostTravel(i, d, tk)
    if (newCost < minCost) { minCost = newCost }
  }
  return minCost
}

// const travelTickets = [
//   [0, 10, 75, 94],
//   [-1, 0, 35, 50],
//   [-1, -1, 0, 80],
//   [-1, -1, -1, 0]
// ]
// console.log(minCostTravel(0, 3, travelTickets))
