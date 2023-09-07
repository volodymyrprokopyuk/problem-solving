import { describe, test, expect } from "vitest"
import { List } from "./list.js"

describe("List", () => {
  test("List from/iterator/length/pop/peek", () => {
    const lst = List.from([1, 2, 3, 4])
    expect(Array.from(lst)).toEqual([4, 3, 2, 1])
    expect(lst.length).toBe(4)
    expect(lst.get(2)).toBe(2)
    expect(lst.get(9)).toBe(undefined)
    expect(lst.delete(1)).toBe(1)
    expect(lst.delete(4)).toBe(4)
    expect(lst.delete(9)).toBe(undefined)
    expect(lst.peek()).toBe(3)
    expect(lst.pop()).toBe(3)
    expect(lst.delete(2)).toBe(2)
    expect(lst.delete(1)).toBe(undefined)
    expect(() => lst.peek()).toThrowError("peek from empty list")
    expect(() => lst.pop()).toThrowError("pop from empty list")
  })
})

describe("List reverse", () => {
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
