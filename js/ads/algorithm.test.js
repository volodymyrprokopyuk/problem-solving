import { describe, it, expect } from "vitest"
import {
  isUnique, isUniqueCmp, bsFind, bsFindRec,
  sortQuickCp
} from "./algorithm.js"

// ** Array

const isUniqueFxt = [
  [[], true], [[1, 2, 3], true], [[1, 2, 1, 3], false]
]

describe("isUnique", () => {
  it.each(isUniqueFxt)(`Case %j`, (arr, exp) => {
    expect(isUnique(arr)).toBe(exp)
  })
})

describe("isUniqueCmp", () => {
  it.each(isUniqueFxt)(`Case %j`, (arr, exp) => {
    expect(isUniqueCmp(arr)).toBe(exp)
  })
})

const bsFindArr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
const bsFindFxt = [
  [[], 99, -1], [[1, 2, 3], 99, -1],
  [bsFindArr, 0, 0], [bsFindArr, 4, 4], [bsFindArr, 9, 9]
]

describe("bsFind", () => {
  it.each(bsFindFxt)("Case %j, %j", (arr, vl, exp) => {
    expect(bsFind(arr, vl)).toBe(exp)
  })
})

describe("bsFindRec", () => {
  it.each(bsFindFxt)("Case %j, %j", (arr, vl, exp) => {
    expect(bsFindRec(arr, vl)).toBe(exp)
  })
})

// ** Sorting

const sortQuickFxt = [
  [[], []], [[1], [1]],
  [[8, 1, 4, 3, 0, 2, 6, 7, 9, 0, 5], [0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]]
]

describe("sortQuickCp", () => {
  it.each(sortQuickFxt)("Case %j", (arr, exp) => {
    expect(sortQuickCp(arr)).toEqual(exp)
  })
})
