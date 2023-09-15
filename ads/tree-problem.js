import { error } from "./util.js"
import { BSTree } from "./tree.js"

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

// const bst = BSTree.from([19, 16, 24, 17, 20, 11, 28, 12, 27, 13])
// console.log(height(bst.root))
// console.log(nodes(bst.root))
// console.log(internalNodes(bst.root))
// console.log(externalNodes(bst.root))
