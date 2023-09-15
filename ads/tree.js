import { inspect } from "util"

export class TNode {
  key; data
  left = null; right = null

  constructor(key, data) { this.key = key; this.data = data }
}

export class BSTree {
  #root = null
  #length = 0
  #cmp

  constructor (cmp = (a, b) => a < b) { this.#cmp = cmp }

  get length() { return this.#length }

  get root() { return this.#root }

  static from(it, cmp) {
    const bst = new BSTree(cmp)
    for (const el of it) {
      Array.isArray(el) ? bst.set(el[0], el[1]) : bst.set(el, el)
    }
    return bst
  }

  [Symbol.iterator]() {
    function* inOrder(nd) {
      if (nd) {
        yield* inOrder(nd.left)
        yield nd
        yield* inOrder(nd.right)
      }
    }
    return inOrder(this.#root)
  }

  [inspect.custom]() {
    const keyData = ({ key, data }) => `${key}: ${data}`
    return `BSTree(${[...this].map(keyData).join(", ")})`
  }

  // O(log(n)) inserts a node into a tree
  set(key, data) {
    const newNode = () => {
      ++this.#length; return new TNode(key, data)
    }
    const set = nd => {
      if (!nd) { return newNode() } // insert
      key === nd.key ? nd.data = data : // update
        this.#cmp(key, nd.key) ? // binary search
        nd.left = set(nd.left) : nd.right = set(nd.right)
      return nd
    }
    this.#root = set(this.#root)
    return this
  }

  // O(log(n)) returns a matching node
  get(key) {
    const get = nd => {
      if (nd) {
        return key === nd.key ? nd : // binary search
          this.#cmp(key, nd.key) ? get(nd.left) : get(nd.right)
      }
    }
    return get(this.#root)
  }

  // O(log(n)) deletes a matching node
  delete(key) {
    const pred = nd => {
      while (nd.right) { nd = nd.right }; return nd
    }
    const succ = nd => {
      while (nd.left) { nd = nd.left }; return nd
    }
    const deleted = nd => dnd || { key: nd.key, data: nd.data }
    const del = (nd, key) => {
      if (nd) {
        if (key === nd.key) {
          // At most one child
          if (!nd.left) { dnd = deleted(nd); --this.#length; return nd.right }
          if (!nd.right) { dnd = deleted(nd); --this.#length; return nd.left }
          // Two children
          dnd = deleted(nd)
          const { key, data } = pred(nd.left)
          nd.key = key; nd.data = data
          nd.left = del(nd.left, key)
          // const { key, data } = succ(nd.right)
          // nd.key = key; nd.data = data
          // nd.right = del(nd.right, key)
        } else {
          this.#cmp(key, nd.key) ? // binary search
            nd.left = del(nd.left, key) : nd.right = del(nd.right, key)
        }
      }
      return nd
    }
    let dnd
    this.#root = del(this.#root, key)
    return dnd
  }

  // O(log(n)) returns a minimum of a tree
  min() {
    if (this.#length === 0) { error("min from empty BSTree") }
    let nd = this.#root
    while (nd.left) { nd = nd.left }
    return nd
  }

  // O(log(n)) returns a maximum of a tree
  max() {
    if (this.#length === 0) { error("max from empty BSTree") }
    let nd = this.#root
    while (nd.right) { nd = nd.right }
    return nd
  }
}
