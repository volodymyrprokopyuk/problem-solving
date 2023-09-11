import { describe, test, expect } from "vitest"
import { List, DList } from "./list.js"

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

describe("DList", () => {
  test("DList from/length/iterator/reverse/push/pop/peek/get/insert/delete", () => {
    let lst = DList.from([1, 2, 3, 4])
    expect(lst.length).toBe(4)
    expect(lst.get(9)).toBe(undefined)
    lst.push(0).pushTail(5)
    expect([lst.peek(), lst.peekTail()]).toEqual([0, 5])
    expect([lst.pop(), lst.popTail()]).toEqual([0, 5])
    lst.insert(lst.get(1), 10)
    lst.insert(lst.get(3), 30)
    lst.insert(lst.get(4), 40)
    expect([...lst]).toEqual([1, 10, 2, 3, 30, 4, 40])
    lst = DList.from([1, 2, 3, 4])
    expect([...lst.reverse]).toEqual([4, 3, 2, 1])
    lst.delete(lst.get(1))
    lst.delete(lst.get(3))
    lst.delete(lst.get(4))
    lst.delete(lst.get(2))
    expect(() => lst.pop()).toThrowError("pop from empty dlist")
    expect(() => lst.popTail()).toThrowError("popTail from empty dlist")
    expect(() => lst.peek()).toThrowError("peek from empty dlist")
    expect(() => lst.peekTail()).toThrowError("peekTail from empty dlist")
    lst.push(1)
    expect(lst.pop()).toBe(1)
    lst.pushTail(1)
    expect(lst.popTail()).toBe(1)
  })
})
