import { describe, test, expect } from "vitest"
import { twoSum } from "./htable-problem.js"

describe("twoSum", () => {
  test("twoSum", () => {
    expect(twoSum([10, 15, 8, 3, 6, 8], 16)).toEqual([[6, 10], [8, 8]])
  })
})
