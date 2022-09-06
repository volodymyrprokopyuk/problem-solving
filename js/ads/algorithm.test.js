import { beforeEach, describe, it, expect } from "vitest"
import { bsFind } from "./algorithm.js"

describe("bsFind edge cases", () => {
  it("should return -1 on an empty array", () => {
    expect(bsFind([])).toEqual(-1)
  })
  it("shold return -1 no not found value", (ctx) => {
    expect(bsFind([1, 2, 3], 99)).toEqual(-1)
  })
})

describe.each([[0, 0], [4, 4], [9, 9]])(
  "bsFind happy path", (vl, expected) => {
  beforeEach((ctx) => {
    ctx.arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  })
  it("should return the correct position", (ctx) => {
    expect(bsFind(ctx.arr, vl)).toEqual(expected)
  })
})
