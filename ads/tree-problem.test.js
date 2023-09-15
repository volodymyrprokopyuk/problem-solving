import { describe, test, expect } from "vitest"
import { BSTree } from "./tree.js"
import {
  height, nodes, internalNodes, externalNodes
} from "./tree-problem.js"

describe("BSTree algorithms", () => {
  test("height", () => {
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
    expect(height(bst.root)).toBe(5)
  })

  test("nodes", () => {
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
    expect(nodes(bst.root)).toBe(10)
  })

  test("internalNodes", () => {
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
    expect(internalNodes(bst.root)).toBe(3)
  })

  test("externalNodes", () => {
    const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
    expect(externalNodes(bst.root)).toBe(4)
  })
})
