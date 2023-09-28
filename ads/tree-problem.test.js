import { describe, test, expect } from "vitest"
import { TNode, BSTree, inOrder, preOrder } from "./tree.js"
import {
  height, nodes, internalNodes, externalNodes, univalCount, univalCount2,
  recPre, evalInfix, minLevelSum, floorCeiling
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

describe("univalCount", () => {
  const n1 = new TNode(0), n2 = new TNode(1), n3 = new TNode(0),
        n4 = new TNode(1), n5 = new TNode(0), n6 = new TNode(1),
        n7 = new TNode(1), n8 = new TNode(0)
  n1.left = n2; n1.right = n3
  n3.left = n4; n3.right = n5
  n4.left = n6; n4.right = n7
  n5.right = n8
  test("univalCount", () => { expect(univalCount(n1)).toBe(6) })
  test("univalCount2", () => { expect(univalCount2(n1)).toBe(6) })
})

describe("recPre", () => {
  test("recPre", () => {
    const pre = ["a", "b", "d", "e", "i", "c", "f", "h", "g"],
          ino = ["d", "b", "e", "i", "a", "h", "f", "c", "g"]
    const tree = recPre(pre, ino)
    expect([...preOrder(tree, true)]).toEqual(pre)
    expect([...inOrder(tree, true)]).toEqual(ino)
  })
})

describe("evalInfix", () => {
  test("evalInfix", () => {
    const n1 = new TNode("*"), n2 = new TNode("+"), n3 = new TNode("-"),
          n4 = new TNode(1), n5 = new TNode(2), n6 = new TNode(7),
          n7 = new TNode(3)
    n1.left = n2, n1.right = n3
    n2.left = n4, n2.right = n5
    n3.left = n6, n3.right = n7
    expect(evalInfix(n1)).toBe(12)
  })
})

describe("minLevelSum", () => {
  test("minLevelSum", () => {
    const n1 = new TNode(7), n2 = new TNode(2), n3 = new TNode(3),
          n4 = new TNode(1), n5 = new TNode(1), n6 = new TNode(3),
          n7 = new TNode(2), n8 = new TNode(4)
    n1.left = n2; n1.right = n3
    n3.left = n4; n3.right = n5
    n4.left = n6; n4.right = n7
    n5.right = n8
    expect(minLevelSum(n1)).toBe(2)
  })
})

describe("floorCeiling", () => {
  const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
  test.each([
    [19, [19, 19]], [18, [17, 19]], [15, [13, 16]], [21, [20, 24]]
  ])("floorCeiling(%j) === %j", (n, exp) => {
    expect(floorCeiling(bst, n)).toEqual(exp)
  })
})
