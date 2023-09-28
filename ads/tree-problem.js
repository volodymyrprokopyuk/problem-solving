import { error } from "./util.js"
import { TNode, BSTree, inOrder, preOrder } from "./tree.js"

// O(n) return a height of a tree
export function height(nd) {
  return nd ? Math.max(height(nd.left), height(nd.right)) + 1 : 0
}

// O(n) returns a number of nodes
export function nodes(nd) {
  return nd ? nodes(nd.left) + nodes(nd.right) + 1 : 0
}

// O(n) returns a number of internal nodes
export function internalNodes(nd) {
  return nd && nd.left && nd.right ?
    internalNodes(nd.left) + internalNodes(nd.right) + 1 : 0
}

// O(n) returns a number of external nodes
export function externalNodes(nd) {
  return nd ? !nd.left && !nd.right ? 1 :
    externalNodes(nd.left) + externalNodes(nd.right) : 0
}

// O(n^2) counts the number of unival subtrees in a tree
export function univalCount(nd) {
  function isUnival(nd, val = nd.key) {
    return !nd ? true :
      nd.key === val ? isUnival(nd.left, val) && isUnival(nd.right, val) : false
  }
  return !nd ? 0 : univalCount(nd.left) + univalCount(nd.right) +
    (isUnival(nd) ? 1 : 0)
}

// O(n) counts the number of unival subtrees in a tree
export function univalCount2(nd) {
  function uvCount(nd) {
    if (!nd) { return true }
    const luv = uvCount(nd.left), ruv = uvCount(nd.right)
    return !luv || !ruv ? false :
      nd.left && nd.left.key !== nd.key ? false :
      nd.right && nd.right.key !== nd.key ? false :
      (++count, true)
  }
  let count = 0
  uvCount(nd)
  return count
}

// O(n) reconstructs a tree from a pre-order and an in-order traversals
export function recPre(pre, ino) {
  if (pre.length === 0) { return null }
  const root = pre[0], ir = ino.indexOf(root)
  const nd = new TNode(root)
  nd.left = recPre(pre.slice(1, ir + 1), ino.slice(0, ir))
  nd.right = recPre(pre.slice(ir + 1), ino.slice(ir + 1))
  return nd
}

// O(n) evaluates an expression tree
export function evalInfix(nd) {
  const ops = {
    "+": (a, b) => a + b, "-": (a, b) => a - b, "*": (a, b) => a * b
  }
  return typeof nd.key === "string" ?
    ops[nd.key](evalInfix(nd.left), evalInfix(nd.right)) : nd.key
}

// O(n) return a tree level with a minimum sum of elements
export function minLevelSum(nd) {
  const levels = []
  function level(nd, lvl) {
    if (nd) {
      if (!levels[lvl]) { levels[lvl] = 0 }
      levels[lvl] += nd.key
      level(nd.left, lvl + 1); level(nd.right, lvl + 1)
    }
  }
  level(nd, 0)
  return Math.min(...levels)
}

// O(log(n)) returns floor and ceiling in a tree of a number
export function floorCeiling(bst, n) {
  function fc(nd, fl, cl) {
    return !nd ? [fl, cl] :
      n < nd.key ? fc(nd.left, fl, nd.key) :
      n > nd.key ? fc(nd.right, nd.key, cl) : [nd.key, nd.key]
  }
  return fc(bst.root)
}

// const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
// console.log(floorCeiling(bst, 18))
// console.log([...inOrder(bst.root, true)])

// const n1 = new TNode(7), n2 = new TNode(2), n3 = new TNode(3),
//       n4 = new TNode(1), n5 = new TNode(1), n6 = new TNode(3),
//       n7 = new TNode(2), n8 = new TNode(4)
// n1.left = n2; n1.right = n3
// n3.left = n4; n3.right = n5
// n4.left = n6; n4.right = n7
// n5.right = n8;
// console.log(minLevelSum(n1))

// const n1 = new TNode("*"), n2 = new TNode("+"), n3 = new TNode("-"),
//       n4 = new TNode(1), n5 = new TNode(2), n6 = new TNode(7),
//       n7 = new TNode(3)
// n1.left = n2, n1.right = n3
// n2.left = n4, n2.right = n5
// n3.left = n6, n3.right = n7
// console.log([...inOrder(n1, true)])
// console.log(evalInfix(n1))

// const pre = ["a", "b", "d", "e", "i", "c", "f", "h", "g"],
//       ino = ["d", "b", "e", "i", "a", "h", "f", "c", "g"]
// const tree = recPre(pre, ino)
// console.log([...preOrder(tree, true)], [...inOrder(tree, true)])

// const n1 = new TNode(0), n2 = new TNode(1), n3 = new TNode(0),
//       n4 = new TNode(1), n5 = new TNode(0), n6 = new TNode(1),
//       n7 = new TNode(1), n8 = new TNode(0)
// n1.left = n2; n1.right = n3
// n3.left = n4; n3.right = n5
// n4.left = n6; n4.right = n7
// n5.right = n8;
// console.log(univalCount(n1))
// console.log(univalCount2(n1))
