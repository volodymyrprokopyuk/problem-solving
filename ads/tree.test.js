import { describe, test, expect } from "vitest"
import { BSTree } from "./tree.js"

describe("BSTree", () => {
  test("BSTree set/get/delete", () => {
    const key = nd => nd?.key
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 20, 28, 12, 27])
    expect(bst.length).toBe(9)
    expect([bst.get(12), bst.get(24), bst.get(-1)].map(key))
      .toEqual([12, 24, undefined])
    expect([...bst].map(key)).toEqual([11, 12, 16, 17, 19, 20, 24, 27, 28])
    expect(
      [bst.delete(17), bst.delete(11), bst.delete(24), bst.delete(-1)].map(key)
    ).toEqual([17, 11, 24, undefined])
    expect([...bst].map(key)).toEqual([12, 16, 19, 20, 27, 28])
  })
})
