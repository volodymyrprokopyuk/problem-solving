import { describe, test, expect } from "vitest"
import {
  findAnagrams, palindromePairs, printZigZag
} from "./string-problem.js"

describe("find anagrams", () => {
  test.each([
    [["ab", "abxabaa"], [0, 3, 4]],
    [["aba", "abaabaa"], [0, 1, 2, 3, 4]]
  ])("findAnagrams(%j, %j) === %j", ([word, str], exp) => {
    expect(findAnagrams(word, str)).toEqual(exp)
  })
})

describe("palindrome pairs", () => {
  test.each([
    [["code", "edoc", "da", "d"], [[0, 1], [1, 0], [2, 3]]]
  ])("%# palindromePiars(%j) === %j", (arr, exp) => {
    expect(palindromePairs(arr)).toEqual(exp)
  })
})

describe("print zig zag", () => {
  test.each([
    ["thisisazigzag", 4, [
      "t     a     g",
      " h   s z   a ",
      "  i i   i z  ",
      "   s     g   ",
    ]]
  ])("%# printZigZag(%j, %j) === %j", (str, k, exp) => {
    expect(printZigZag(str, k).map(r => r.join(""))).toEqual(exp)
  })
})
