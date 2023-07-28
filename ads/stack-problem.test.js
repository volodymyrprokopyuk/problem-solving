import { describe, test, expect } from "vitest"
import { checkParens } from "./stack-problem.js"

describe("stack check parens", () => {
  test.each([
    ["a(b[c{d}])"]
  ])("%# checkParens(%j) === true", (str) => {
    expect(checkParens(str)).toBe(true)
  })

  test.each([
    ["a(b[c{d)])", "invalid close ) expected }"],
    ["a(b[c{d}])]", "extra close ]"],
    ["a(b[c{d}])[", "extra paren ["]
  ])("%# checkParens(%j) => error", (str, err) => {
    expect(() => checkParens(str)).toThrowError(err)
  })
})
