import { describe, test, expect } from "vitest"
import { Stack } from "./stack.js"

describe("Stack", () => {
  test("push/pop/peek", () => {
    const arr = [1, 2, 3]
    const stk = Stack.from(arr)
    const pop = []
    while (stk.length > 0) { pop.push(stk.pop()) }
    expect(pop).toEqual(arr.toReversed())
    stk.push(4).push(5)
    expect(stk.pop()).toBe(5)
    expect(stk.peek()).toBe(4)
    expect(stk.length).toBe(1)
  })
})
