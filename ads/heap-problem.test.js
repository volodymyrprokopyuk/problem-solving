import { describe, test, expect } from "vitest"
import { HTable } from "./htable.js"
import { runningMedian, regularNumbers, huffmanEncode } from "./heap-problem.js"

describe("runningMedian", () => {
  test.each([
    [[2, 1, 5, 7, 2, 0, 5], [2, 1.5, 2, 3.5, 2, 2, 2]]
  ])("%# runningMedian(%j) === %j", (arr, exp) => {
    expect(runningMedian(arr)).toEqual(exp)
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
