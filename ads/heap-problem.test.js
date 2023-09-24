import { describe, test, expect } from "vitest"
import { HTable } from "./htable.js"
import {
  runningMedian, topSimilarSites, regularNumbers, huffmanEncode
} from "./heap-problem.js"

describe("runningMedian", () => {
  test.each([
    [[2, 1, 5, 7, 2, 0, 5], [2, 1.5, 2, 3.5, 2, 2, 2]]
  ])("%# runningMedian(%j) === %j", (arr, exp) => {
    expect(runningMedian(arr)).toEqual(exp)
  })
})

describe("topSimilarSites", () => {
  test("topSimilarSites", () => {
    const accessLog = [
      ["g", 1], ["g", 3], ["g", 5], ["p", 1], ["p", 2], ["y", 6], ["y", 2],
      ["y", 3], ["y", 4], ["y", 5], ["w", 4], ["w", 5], ["w", 6], ["w", 7],
      ["b", 1], ["b", 3], ["b", 5], ["b", 6]
    ]
    const exp = [
      { sim: 0.75, pair: ["b", "g"] }, { sim: 0.5, pair: ["w", "y"] }
    ]
    expect(topSimilarSites(2, accessLog)).toEqual(exp)
  })
})

describe("regularNumbers", () => {
  test("regularNumbers", () => {
    expect(regularNumbers(10)).toEqual([1, 2, 3, 4, 5, 6, 8, 9, 10, 12])
  })
})

describe("huffmanEncode", () => {
  test.each([
    [[["a", 3], ["c", 6], ["e", 8], ["f", 2]],
     [["a", "101"], ["c", "11"], ["e", "0"], ["f", "100"]]]
  ])("%# huffmanEncode(%j) === %j", (freq, exp) => {
    expect(huffmanEncode(freq).equal(HTable.from(exp))).toBe(true)
  })
})
