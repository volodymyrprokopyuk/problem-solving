import { describe, test, expect } from "vitest"
import { runningMedian } from "./heap-problem.js"

describe("runningMedian", () => {
  test.each([
    [[2, 1, 5, 7, 2, 0, 5], [2, 1.5, 2, 3.5, 2, 2, 2]]
  ])("%# runningMedian(%j) === %j", (arr, exp) => {
    expect(runningMedian(arr)).toEqual(exp)
  })
})
