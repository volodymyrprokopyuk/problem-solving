import { describe, test, expect } from "vitest"
import { List } from "./list.js"

describe("list from iterable + iterator + length", () => {
  test.each([
    [[1, 2, 3], [3, 2, 1]]
  ])("%# List.frmo(%j) === %j", (it, exp) => {
    const lst = List.from(it)
    expect(Array.from(lst)).toEqual(exp)
    expect(lst.length).toBe(exp.length)
  })
})
