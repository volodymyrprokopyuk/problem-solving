import { describe, test, expect } from "vitest"
import { Stack } from "./stack.js"

describe("Stack", () => {
  test("Stack push/pop/peek", () => {
    const stk = Stack.from([1, 2, 3])
    expect(stk.length).toBe(3)
    expect([...stk]).toEqual([3, 2, 1])
    expect([stk.peek(), stk.pop()]).toEqual([3, 3])
    stk.pop(); stk.pop()
    expect(() => stk.peek()).toThrowError("peek from empty stack")
    expect(() => stk.pop()).toThrowError("pop from empty stack")
  })
})
