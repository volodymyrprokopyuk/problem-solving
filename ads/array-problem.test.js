import { describe, test, expect } from "vitest"
import { pascalTriangle, pascalTriangle2 } from "./array-problem.js"

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
])(`%# array pascal triangle (%j) === %j`, (n, exp) => {
  test(`pascalTriangle`, () => {
    expect(pascalTriangle(n)).toEqual(exp)
  })
  test(`pascalTriangle2`, () => {
    expect(pascalTriangle2(n)).toEqual(exp)
  })
})
