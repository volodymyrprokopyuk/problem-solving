import { error } from "./util.js"
import { TNode, BSTree } from "./tree.js"

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

const n1 = new TNode(0), n2 = new TNode(1), n3 = new TNode(0),
      n4 = new TNode(1), n5 = new TNode(0), n6 = new TNode(1),
      n7 = new TNode(1), n8 = new TNode(0)
n1.left = n2; n1.right = n3
n3.left = n4; n3.right = n5
n4.left = n6; n4.right = n7
n5.right = n8;

// console.log(univalCount(n1))
// console.log(univalCount2(n1))
