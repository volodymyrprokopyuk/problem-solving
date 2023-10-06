import { describe, test, expect } from "vitest"
import { matrix } from "./array.js"
import {
  pascalTriangle, pascalTriangle2,
  mxAdd, mxKMul, mxMul, mxTrans, mxFillDiag, mxDiags,
  exKthBigSmall, productAllOther, productAllOther2,
  smallestWindowToSort, smallestWindowToSort2,
  maxSubArraySum, maxSubArraySum2, maxSubArraySum3, smallerToTheRight,
  linear3ColorSort, pancakeSort
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

describe("matrix fill diagonal", () => {
  test.each([
    [matrix(2), [[ 0, 1], [-1, 0]]]
  ])("%# mxFillDiag(%j) === %j", (mx, exp) => {
    expect(mxFillDiag(mx)).toEqual(exp)
  })
})

describe("matrix get diagonals", () => {
  test.each([
    [
      [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6], [4, 5, 6, 7]],
      [[1, 3, 5, 7], [4, 4, 4, 4]]
    ]
  ])("%# mxDiag(%j) === %j", (mx, exp) => {
    expect(mxDiags(mx)).toEqual(exp)
  })
})

describe("exchange k-th biggest/smallest", () => {
  test.each([
    [[1], 2, undefined],
    [[1, 2, 3, 4, 5, 6, 7, 8, 9], 0, [9, 2, 3, 4, 5, 6, 7, 8, 1]],
    [[1, 2, 3, 4, 5, 6, 7, 8, 9], 3, [1, 2, 3, 6, 5, 4, 7, 8, 9]],
    [[1, 2, 3, 4, 5, 6, 7, 8, 9], 7, [1, 8, 3, 4, 5, 6, 7, 2, 9]]
  ])("%# exKthBigSmall(%j, %j) === %j", (arr, k, exp) => {
    expect(exKthBigSmall(arr, k)).toEqual(exp)
  })
})

describe.each([
  [[1, 2, 3, 4, 5], [120, 60, 40, 30, 24]]
])("%# product all other (%j) === %j", (arr, exp) => {
  test("productAllOther", () => {
    expect(productAllOther(arr)).toEqual(exp)
  })
  test("productAllOther2", () => {
    expect(productAllOther2(arr)).toEqual(exp)
  })
})

describe.each([
  [[1, 2, 3, 7, 4, 6, 5, 8, 9, 10], [3, 6]]
])("%# smallest window to sort (%j) === %j", (arr, exp) => {
  test("smallestWindowToSort", () => {
    expect(smallestWindowToSort(arr)).toEqual(exp)
  })
  test("smallestWindowToSort2", () => {
    expect(smallestWindowToSort2(arr)).toEqual(exp)
  })
})

describe.each([
  [[1, -5, 2, -3, 8, 9], [17, 4, 5]],
  [[34, -50, 42, 14, -5, 86], [137, 2, 5]],
  [[-1, -2], [0, -1, -1]]
])("%# max sub array sum (%j) === %j", (arr, exp) => {
  test("maxSubArraySum", () => {
    expect(maxSubArraySum(arr)).toEqual(exp)
  })
  test("maxSubArraySum2", () => {
    expect(maxSubArraySum2(arr)).toEqual(exp)
  })
  test("maxSubArraySum3", () => {
    expect(maxSubArraySum3(arr)).toEqual(exp)
  })
})

describe("number of smaller elements to the right", () => {
  test.each([
    [[3, 4, 9, 6, 1], [1, 1, 2, 1, 0]]
  ])("%# smallerToTheRight(%j) === %j", (arr, exp) => {
    expect(smallerToTheRight(arr)).toEqual(exp)
  })
})

describe("linear3ColorSort", () => {
  test("linear3ColorSort", () => {
    const arr = ["g", "r", "b", "r", "b", "g", "r", "b", "g"]
    linear3ColorSort(arr)
    expect(arr).toEqual(["r", "r", "r", "g", "g", "g", "b", "b", "b"])
  })
})

describe("pancakeSort", () => {
  test("pancakeSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    pancakeSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})
