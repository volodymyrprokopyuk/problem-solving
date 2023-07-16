import { describe, test, expect } from "vitest"
import { List } from "./list.js"
import { reverse } from "./list-problem.js"

describe("list reverse", () => {
  test.each([
    [List.from([1, 2, 3]), [1, 2, 3]]
  ])("%# reverse(%j) === %j", (lst, exp) => {
    expect(Array.from(reverse(lst))).toEqual(exp)
  })
})
