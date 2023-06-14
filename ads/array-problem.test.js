import { describe, test, expect } from "vitest"
import {
  pascalTriangle, pascalTriangle2,
  mxAdd, mxKMul, mxMul, mxTrans
} from "./array-problem.js"

describe.each([
    [0, [[1]]],
    [1, [[1], [1, 1]]],
    [2, [[1], [1, 1], [1, 2, 1]]],
    [5, [
      [ 1 ],
      [ 1, 1 ],
      [ 1, 2, 1 ],
      [ 1, 3, 3, 1 ],
      [ 1, 4, 6, 4, 1 ],
      [ 1, 5, 10, 10, 5, 1 ]
    ]]
])("%# array pascal triangle (%j) === %j", (n, exp) => {
  test("pascalTriangle", () => {
    expect(pascalTriangle(n)).toEqual(exp)
  })
  test("pascalTriangle2", () => {
    expect(pascalTriangle2(n)).toEqual(exp)
  })
})

describe("matrix kMul", () => {
  test.each([
    [2, [1], "mxKMul: not a matrix"]
  ])("%# mxKMul(%j, %j) => error %j", (k, a, exp) => {
    expect(() => mxKMul(k, a)).toThrowError(exp)
  })
  test.each([
    [2, [[1, 2], [3, 4]], [[2, 4], [6, 8]]]
  ])("%# mxKMul(%j, %j) === %j", (k, a, exp) => {
    expect(mxKMul(k, a)).toEqual(exp)
  })
})

describe("matrix add", () => {
  test.each([
    [[1], [2, 3], "mxAdd: incompatible matrices"]
  ])("%# mxAdd(%j, %j) => error %j", (a, b, exp) => {
    expect(() => mxAdd(a, b)).toThrowError(exp)
  })
  test.each([
    [[[1, 2], [3, 4]], [[5, 6], [7, 8]], [[6, 8], [10, 12]]]
  ])("%# mxAdd(%j, %j) === %j", (a, b, exp) => {
    expect(mxAdd(a, b) === exp)
  })
})

describe("matrix mul", () => {
  test.each([
    [[[1], [2]], [[1, 2], [3, 4]], "mxMul: incompatible matrices"]
  ])("%# mxMul(%j, %j) => error %j", (a, b, exp) => {
    expect(() => mxMul(a, b)).toThrowError(exp)
  })
  test.each([
    [[[1, 2, 3], [3, 2, 1]], [[4, 7], [5, 8], [6, 9]], [[32, 50], [28, 46]]]
  ])("%# mxMul(%j, %j) === %j", (a, b, exp) => {
    expect(mxMul(a, b)).toEqual(exp)
  })
})

describe("matrix trans", () => {
  test.each([
    [[1], "mxTrans: not a matrix"]
  ])("%# mxTrans(%j) => error %j", (a, exp) => {
    expect(() => mxTrans(a)).toThrowError(exp)
  })
  test.each([
    [[[1, 2, 3], [4, 5, 6]], [[1, 4], [2, 5], [3, 6]]]
  ])("%# mxTrans(%j) === %j", (a, exp) => {
    expect(mxTrans(a)).toEqual(exp)
  })
})
