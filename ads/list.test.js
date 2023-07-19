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

describe("list reverse", () => {
  test.each([
    [List.from([]), []],
    [List.from([1]), [1]],
    [List.from([1, 2, 3]), [1, 2, 3]],
    [List.from([1, 2, 3, 4]), [1, 2, 3, 4]]
  ])("%# reverse(%j) === %j", (lst, exp) => {
    lst.reverse()
    expect(Array.from(lst)).toEqual(exp)
  })
})
