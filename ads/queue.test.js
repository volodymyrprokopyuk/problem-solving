import { describe, test, expect } from "vitest"
import { Queue, Deque } from "./queue.js"

describe("Queue", () => {
  test("Queue enq/deq/peek", () => {
    const que = Queue.from([1, 2, 3])
    expect(que.length).toBe(3)
    expect([...que]).toEqual([1, 2, 3])
    expect([que.peek(), que.deq()]).toEqual([1, 1])
    que.deq(); que.deq()
    expect(() => que.peek()).toThrowError("peek from empty queue")
    expect(() => que.deq()).toThrowError("deq from empty queue")
  })
})

describe("Deque", () => {
  test("Deque enq(Front)/deq(Rear)/peek(Rear)", () => {
    const deq = Deque.from([1, 2, 3])
    expect(deq.length).toBe(3)
    expect([...deq]).toEqual([1, 2, 3])
    deq.enq(4).enqFront(0)
    expect([deq.peek(), deq.deq()]).toEqual([0, 0])
    expect([deq.peekRear(), deq.deqRear()]).toEqual([4, 4])
    deq.deq(); deq.deq(); deq.deq()
    expect(() => deq.deq()).toThrowError("deq from empty deque")
    expect(() => deq.deqRear()).toThrowError("deqRear from empty deque")
    expect(() => deq.peek()).toThrowError("peek from empty deque")
    expect(() => deq.peekRear()).toThrowError("peekRear from empty deque")
    deq.enqFront(1)
    expect(deq.deqRear()).toBe(1)
  })
})
