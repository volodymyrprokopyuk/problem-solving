import { describe, test, expect } from "vitest"
import { List, DList } from "./list.js"

describe("List", () => {
  test("List push/pop/peek/get/delete/reverse", () => {
    const lst = List.from([1, 2, 3, 4])
    expect(lst.length).toBe(4)
    lst.push(5)
    expect([lst.peek(), lst.pop()]).toEqual([5, 5])
    expect([lst.get(2)?.value, lst.get(-1)]).toEqual([2, undefined])
    lst.reverse()
    expect([...lst]).toEqual([1, 2, 3, 4])
    expect([lst.delete(3), lst.delete(1), lst.delete(4), lst.delete(-1)])
      .toEqual([3, 1, 4, undefined])
    lst.delete(2)
    expect(() => lst.peek()).toThrowError("peek from empty list")
    expect(() => lst.pop()).toThrowError("pop from empty list")
  })
})

describe("DList", () => {
  test("DList push(Tail)/pop(Tail)/peek(Tail)/get/insert/delete", () => {
    const value = nd => nd.value
    let lst = DList.from([1, 2, 3, 4])
    expect(lst.length).toBe(4)
    expect([...lst.values(true)]).toEqual([4, 3, 2, 1])
    expect([...lst.entries(true)].map(value)).toEqual([4, 3, 2, 1])
    lst.push(0)
    expect([lst.peek(), lst.pop()]).toEqual([0, 0])
    lst.pushTail(5)
    expect([lst.peekTail(), lst.popTail()]).toEqual([5, 5])
    expect([lst.get(3)?.value, lst.get(-1)]).toEqual([3, undefined])
    lst.delete(lst.get(3)); lst.delete(lst.get(1))
    lst.delete(lst.get(4)); lst.pop()
    lst.push(1)
    lst.insert(lst.get(1), 3); lst.insert(lst.get(1), 2)
    expect([...lst]).toEqual([1, 2, 3])
    lst.delete(lst.get(2)); lst.delete(lst.get(3)); lst.popTail()
    lst.push(1); lst.delete(lst.get(1))
    expect(() => lst.pop()).toThrowError("pop from empty dlist")
    expect(() => lst.popTail()).toThrowError("popTail from empty dlist")
    expect(() => lst.peek()).toThrowError("peek from empty dlist")
    expect(() => lst.peekTail()).toThrowError("peekTail from empty dlist")
  })
})
