import { describe, test, expect } from "vitest"
import { Queue } from "./queue.js"

describe("Queue", () => {
  test("enq/deq/peek", () => {
    const arr = [1, 2, 3]
    const que = Queue.from(arr)
    const deq = []
    while (que.length > 0) { deq.push(que.deq()) }
    expect(deq).toEqual(arr)
    que.enq(4).enq(5)
    expect(que.deq()).toBe(4)
    expect(que.peek()).toBe(5)
    expect(que.length).toBe(1)
  })
})
