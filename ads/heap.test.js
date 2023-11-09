import { describe, test, expect } from "vitest"
import { Heap } from "./heap.js"

describe("Heap", () => {
  test("Heap from/length/iterator/push/pop/peek", () => {
    const heap = Heap.from([11, 27, 17, 19, 15, 17, 31, 40])
    expect(heap.length).toBe(8)
    expect(heap.peek()).toBe(11)
    expect([...heap]).toEqual([11, 15, 17, 17, 19, 27, 31, 40])
    expect(() => heap.peek()).toThrowError("peek from empty heap")
    expect(() => heap.pop()).toThrowError("pop from empty heap")
  })
  test("Heap decKey", () => {
    const heap = Heap.from(
      [11, 27, 17, 19, 15, 18, 31, 40], (a, b) => a < b, a => a
    )
    heap.decKey(40, 10)
    expect(heap.peek()).toBe(10)
    heap.decKey(18, 9)
    expect(heap.peek()).toBe(9)
    expect(() => heap.decKey(99, 0)).toThrow("key 99 is not in heap")
    expect([...heap]).toEqual([9, 10, 11, 15, 17, 19, 27, 31])
  })
})
