import { describe, test, expect } from "vitest"
import { merge } from "./array.js"

describe("array merge", () => {
  test.each([
    [[], [], []],
    [[1, 3], [2], [1, 2, 3]],
    [[1, 4, 7], [2, 3, 5, 8, 9], [1, 2, 3, 4, 5, 7, 8, 9]]
  ])(`%# merge(%o, %o) === %o`, (a, b, exp) => {
    expect(merge(a, b)).toEqual(exp)
  })
})
