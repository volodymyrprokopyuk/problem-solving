import { describe, test, expect } from "vitest"
import { Heap } from "./heap.js"

describe("Heap", () => {
  test("push/pop/peek", () => {
    const heap = Heap.from([11, 27, 17, 19, 15, 17, 31, 40])
    expect(heap.peek()).toBe(40)
    expect(heap.length).toBe(8)
    const max = []
    while (heap.length > 0) { max.push(heap.pop()) }
    expect(max).toEqual([40, 31, 27, 19, 17, 17, 15, 11])
  })
})
