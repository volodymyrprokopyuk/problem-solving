import { describe, test, expect } from "vitest"
import { BSTree, preOrder, postOrder } from "./tree.js"

describe("BSTree", () => {
  test("BSTree set/get/delete/min/max", () => {
    const key = nd => nd?.key
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 20, 28, 12, 27])
    expect(bst.length).toBe(9)
    expect([bst.get(12), bst.get(24), bst.get(-1)].map(key))
      .toEqual([12, 24, undefined])
    expect([...bst].map(key)).toEqual([11, 12, 16, 17, 19, 20, 24, 27, 28])
    expect([...preOrder(bst.root, true)])
      .toEqual([19, 16, 11, 12, 17, 24, 20, 28, 27])
    expect([...postOrder(bst.root, true)])
      .toEqual([12, 11, 17, 16, 20, 27, 28, 24, 19])
    expect(
      [bst.delete(17), bst.delete(11), bst.delete(24), bst.delete(-1)]
        .map(kv => kv && kv[0])
    ).toEqual([17, 11, 24, undefined])
    expect([...bst].map(key)).toEqual([12, 16, 19, 20, 27, 28])
    expect([bst.min(), bst.max()].map(key)).toEqual([12, 28])
  })
})
