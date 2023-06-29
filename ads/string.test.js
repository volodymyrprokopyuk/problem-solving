import { describe, test, expect } from "vitest"
import { reverse, find } from "./string.js"

describe("string reverse", () => {
  test.each([
    ["abcd", "dcba"], ["abcde", "edcba"]
  ])("%# reverse(%j) === %j", (str, exp) => {
    expect(reverse(str)).toBe(exp)
  })
})

describe("string find", () => {
  test.each([
    ["abcde", "bcd", 1], ["abcde", "bcde", 1],  ["abcde", "bcdx", -1]
  ])("%# find(%j, %j) === %j", (str, sub, exp) => {
    expect(find(str, sub)).toBe(exp)
  })
})
