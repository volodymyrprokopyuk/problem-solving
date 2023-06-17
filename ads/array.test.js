import { describe, test, expect } from "vitest"
import { merge, hoarePartition } from "./array.js"

describe("array merge", () => {
  test.each([
    [[], [], []],
    [[1, 3], [2], [1, 2, 3]],
    [[1, 4, 7], [2, 3, 5, 8, 9], [1, 2, 3, 4, 5, 7, 8, 9]]
  ])("%# merge(%j, %j) === %j", (a, b, exp) => {
    expect(merge(a, b)).toEqual(exp)
  })
})

describe.each([
  [[], undefined, []],
  [[1], 0, [1]],
  [[1, 2], 0, [1, 2]],
  [[2, 1], 1, [1, 2]],
  [[2, 3, 1], 1, [1, 2, 3]],
  [[2, 1, 3, 2], 2, [2, 1, 2, 3]],
  [[5, 9, 1, 3, 7, 8, 4, 6, 2], 4, [4, 2, 1, 3, 5, 8, 7, 6, 9]]
])("%# partition scheme (%j) === [%j, %j]", (arr, expI, expArr) => {
  test("hoarePartition", () => {
    const a = arr.slice(), i = hoarePartition(a)
    expect(i).toBe(expI)
    expect(a).toEqual(expArr)
  })
})
