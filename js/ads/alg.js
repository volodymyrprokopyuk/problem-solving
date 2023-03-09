function lt(a, b) { return a < b }
function gt(a, b) { return a > b }

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

// O(n^2)
export function bubbleSort(arr, cmp = lt) {
  for (let i = arr.length - 1; i > 0; --i) {
    let swapped = false
    for (let j = 0; j < i; ++j) {
      if (cmp(arr[j + 1], arr[j])) {
        swap(arr, j, j + 1)
        swapped = true
      }
    }
    if (!swapped) { break }
  }
}

const arrs = [
  [], [0], [1, 2], [3, 2, 1],
  [9, 0, 2, 4, 6, 3, 8, 7, 1, 5, 0]
]

arrs.forEach(arr => { bubbleSort(arr, gt); console.log(arr) })
