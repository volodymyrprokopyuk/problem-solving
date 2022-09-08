import { beforeEach, describe, it, expect } from "vitest"
import { isUnique, isUniqueCmp, bsFind, sortQuickCp } from "./algorithm.js"

// ** Array

describe.each(
  [[[], true], [[1, 2, 3], true], [[1, 2, 1, 3], false]]
)("isUnique / isUniqueCmp", (arr, expected) => {
  it("isUnique should identify duplicates", () => {
    expect(isUnique(arr)).toBe(expected)
  })
  it("isUniqueCmp should identify duplicates", () => {
    expect(isUniqueCmp(arr)).toBe(expected)
  })
})

describe("bsFind edge cases", () => {
  it("should return -1 on an empty array", () => {
    expect(bsFind([])).toEqual(-1)
  })
  it("shold return -1 no not found value", (ctx) => {
    expect(bsFind([1, 2, 3], 99)).toEqual(-1)
  })
})

describe.each([[0, 0], [4, 4], [9, 9]])(
  "bsFind happy path", (vl, expected) => {
  beforeEach((ctx) => {
    ctx.arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  })
  it("should return the correct position", (ctx) => {
    expect(bsFind(ctx.arr, vl)).toEqual(expected)
  })
})

// ** Sorting

describe("sortQuickCp", () => {
  it.each(
    [[[], []], [[1], [1]],
     [[8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5], [0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]]
  )("should return sorted copy", (arr, expected) => {
    expect(sortQuickCp(arr)).toEqual(expected)
  })
})
