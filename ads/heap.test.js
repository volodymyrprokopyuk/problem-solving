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
})
