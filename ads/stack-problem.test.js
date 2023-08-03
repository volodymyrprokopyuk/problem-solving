import { describe, test, expect } from "vitest"
import {
  checkParens,
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
