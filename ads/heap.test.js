import { describe, test, expect } from "vitest"
import { Heap } from "./heap.js"

describe("Heap", () => {
  test("Heap from/length/iterator/push/pop/peek", () => {
    const key = ([key, _]) => key
    const heap = Heap.from([11, 27, 17, 19, 15, 17, 31, 40])
    expect(heap.length).toBe(8)
    expect(heap.peek()[0]).toBe(40)
    expect([...heap].map(key)).toEqual([40, 31, 27, 19, 17, 17, 15, 11])
    expect(() => heap.peek()).toThrowError("peek from empty heap")
    expect(() => heap.pop()).toThrowError("pop from empty heap")
    const heap2 = Heap.from([[1, "a"], [3, "c"], [2, "b"]], (a, b) => a > b)
    expect([...heap2]).toEqual([[1, "a"], [2, "b"], [3, "c"]])
  })
})
