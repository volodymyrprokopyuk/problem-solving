import { describe, test, expect } from "vitest"
import { Heap } from "./heap.js"

describe("Heap", () => {
  test("Heap from/length/iterator/push/pop/peek", () => {
    const heap = Heap.from([11, 27, 17, 19, 15, 17, 31, 40])
    expect(heap.length).toBe(8)
    expect(heap.peek()).toBe(40)
    expect([...heap]).toEqual([40, 31, 27, 19, 17, 17, 15, 11])
    expect(() => heap.peek()).toThrowError("peek from empty heap")
    expect(() => heap.pop()).toThrowError("pop from empty heap")
    expect([...Heap.from([1, 3, 2], (a, b) => a > b)]).toEqual([1, 2, 3])
  })
})
