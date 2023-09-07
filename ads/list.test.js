import { describe, test, expect } from "vitest"
import { List } from "./list.js"

describe("List", () => {
  test("List from/length/iterator/push/pop/peek/get/delete/reverse", () => {
    const lst = List.from([1, 2, 3, 4])
    lst.reverse()
    expect([...lst]).toEqual([1, 2, 3, 4])
    expect(lst.length).toBe(4)
    expect(lst.get(2).data).toBe(2)
    expect(lst.get(9)).toBe(undefined)
    expect(lst.delete(4)).toBe(4)
    expect(lst.delete(1)).toBe(1)
    expect(lst.delete(9)).toBe(undefined)
    expect(lst.peek()).toBe(2)
    expect(lst.pop()).toBe(2)
    expect(lst.delete(3)).toBe(3)
    expect(lst.delete(1)).toBe(undefined)
    expect(() => lst.peek()).toThrowError("peek from empty list")
    expect(() => lst.pop()).toThrowError("pop from empty list")
  })
})
