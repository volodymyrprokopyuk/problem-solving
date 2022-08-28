// ** Hash problems

/* ** Two-sum problem
 * Given a list of numbers and a number k, return whether any two numbers from
 * the list add up to k
 */

// O(n^2) - sums only distinct numbers
export function twoSum(arr, k) {
  for (let i = 0; i < arr.length; ++i) {
    for (let j = 0; j < arr.length; ++j) {
      if (i !== j && arr[i] + arr[j] === k) { return true }
    }
  }
  return false
}

// console.log(twoSum([10, 15, 3, 7], 20))

// O(n) - sums also the number to itself
export function twoSum2(arr, k) {
  const ht = { }
  for (const el of arr) {
    ht[el] = true
    if (k - el in ht) { return true }
  }
  return false
}

// console.log(twoSum2([10, 15, 3, 7], 20))
