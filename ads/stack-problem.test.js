import { describe, test, expect } from "vitest"
import {
  checkParens, MaxStack,
  infixToPostfix, postfixEvaluate, infixToPrefix, prefixEvaluate
} from "./stack-problem.js"

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

const algExpr = [
  "a + b * c ^ d", "a ^ b * c + d",
  "(a + b) * c ^ d", "a + (b * c) ^ d", "a ^ b * (c + d)",
  "a ^ (b * (c + d))", "((a + b) * c) ^ d",
  "a - (b + c * d)", "a - (b * c + d)"
]

describe("stack infix to postfix convert", () => {
  const expected = [
    "a b c d ^ * +", "a b ^ c * d +",
    "a b + c d ^ *", "a b c * d ^ +", "a b ^ c d + *",
    "a b c d + * ^", "a b + c * d ^",
    "a b c d * + -", "a b c * d + -"
  ]
  const pairs = []
  for (let i = 0; i < algExpr.length; ++i) {
    pairs.push([algExpr[i], expected[i]])
  }
  test.each(pairs)("%# infixToPostfix(%j) === %j", (infix, exp) => {
    expect(infixToPostfix(infix)).toBe(exp)
  })
})

describe("stack infix to prefix convert", () => {
  const expected = [
    "+ a * b ^ c d", "+ * ^ a b c d",
    "* + a b ^ c d", "+ a ^ * b c d", "* ^ a b + c d",
    "^ a * b + c d", "^ * + a b c d",
    "- a + b * c d", "- a + * b c d"
  ]
  const pairs = []
  for (let i = 0; i < algExpr.length; ++i) {
    pairs.push([algExpr[i], expected[i]])
  }
  test.each(pairs)("%# infixToPrefix(%j) === %j", (infix, exp) => {
    expect(infixToPrefix(infix)).toBe(exp)
  })
})

const arithExpr = [
  ["1 + 2 * 3 ^ 4", 163],
  ["1 ^ 2 * 3 + 4", 7],
  ["(1 + 2) * 3 ^ 4", 243],
  ["1 + (2 * 3) ^ 4", 1297],
  ["1 ^ 2 * (3 + 4)", 7],
  ["1 ^ (2 * (3 + 4))", 1],
  ["((1 + 2) * 3) ^ 4", 6561],
  ["1 - (2 + 3 * 4)", -13],
  ["1 - (2 * 3 + 4)", -9]
]

describe("stack postfix evaluate", () => {
  test.each(arithExpr)("%# postfixEvaluate(%j) === %j", (infix, exp) => {
    expect(postfixEvaluate(infixToPostfix(infix))).toBe(exp)
  })
})

describe("stack prefix evaluate", () => {
  test.each(arithExpr)("%# prefixEvaluate(%j) === %j", (infix, exp) => {
    expect(prefixEvaluate(infixToPrefix(infix))).toBe(exp)
  })
})

describe("MaxStack", () => {
  test.each([
    [[5, 3, 1, 6, 2, 8], [8, 6, 6, 5, 5, 5]]
  ])("%# push/max/pop", (arr, exp) => {
    const stk = new MaxStack()
    arr.forEach(el => stk.push(el))
    const max = [], pop = []
    while (stk.length > 0) { max.push(stk.max()); pop.push(stk.pop()) }
    expect(max).toEqual(exp)
    expect(pop).toEqual(arr.toReversed())
  })
})
