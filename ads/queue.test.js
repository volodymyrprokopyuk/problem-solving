import { describe, test, expect } from "vitest"
import { Queue, Deque } from "./queue.js"

describe("Queue", () => {
  test("Queue from/iterator/length/enq/deq/peek", () => {
    const que = Queue.from([1, 2, 3])
    const deq = []
    while (que.length > 0) { deq.push(que.deq()) }
    expect(deq).toEqual([1, 2, 3])
    que.enq(4).enq(5)
    expect(que.deq()).toBe(4)
    expect(que.peek()).toBe(5)
    expect(que.deq()).toBe(5)
    expect(() => que.peek()).toThrowError("peek from empty queue")
    expect(() => que.deq()).toThrowError("deq from empty queue")
    expect([...Queue.from([1, 2, 3])]).toEqual([1, 2, 3])
  })
})

describe("Deque", () => {
  test("Deque from/iterator/length/enq/enqFront/deq/deqRear/peek/peekRear", () => {
    const deq = Deque.from([1, 2, 3])
    deq.enq(4).enqFront(0)
    expect([...deq]).toEqual([0, 1, 2, 3, 4])
    expect([deq.deq(), deq.deqRear()]).toEqual([0, 4])
    expect(deq.length).toBe(3)
    expect([deq.peek(), deq.peekRear()]).toEqual([1, 3])
    deq.deq(); deq.deq(); deq.deqRear()
    expect(() => deq.deq()).toThrowError("deq from empty deque")
    expect(() => deq.deqRear()).toThrowError("deqRear from empty deque")
    expect(() => deq.peek()).toThrowError("peek from empty deque")
    expect(() => deq.peekRear()).toThrowError("peekRear from empty deque")
    deq.enqFront(1)
    expect(deq.deq()).toBe(1)
  })
})
