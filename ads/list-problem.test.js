import { describe, test, expect } from "vitest"
import { List } from "./list.js"
import { addNumbers } from "./list-problem.js"

describe("list add numbers", () => {
  test.each([
    [List.from([9, 9]), List.from([1, 2, 5]), [4, 2, 2]],
    [List.from([1, 2, 7]), List.from([8, 9]), [6, 1, 2]],
    [List.from([9]), List.from([9]), [8, 1]]
  ])("%# addNumbers(%j, %j) === %j", (a, b, exp) => {
    expect(Array.from(addNumbers(a, b))).toEqual(exp)
  })
})
