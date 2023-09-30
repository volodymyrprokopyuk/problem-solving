import { inspect } from "util"
import { HTable } from "./htable.js"

export class TNode {
  key; value
  left = null; right = null

  constructor(key, value) { this.key = key; this.value = value }
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

  [Symbol.iterator]() { return inOrder(this.#root) }

  [inspect.custom]() {
    const keyValue = ({ key, value }) => `${key}: ${value}`
    return `BSTree(${[...this].map(keyValue).join(", ")})`
  }

  // O(log(n)) inserts a node into a tree
  set(key, value) {
    const newNode = () => {
      ++this.#length; return new TNode(key, value)
    }
    const set = nd => {
      if (!nd) { return newNode() } // insert
      key === nd.key ? nd.value = value : // update
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
    const deleted = nd => dnd || [nd.key, nd.value]
    const del = (nd, key) => {
      if (nd) {
        if (key === nd.key) {
          // At most one child
          if (!nd.left) { dnd = deleted(nd); --this.#length; return nd.right }
          if (!nd.right) { dnd = deleted(nd); --this.#length; return nd.left }
          // Two children
          dnd = deleted(nd)
          const { key, value } = pred(nd.left)
          nd.key = key; nd.value = value
          nd.left = del(nd.left, key)
          // const { key, value } = succ(nd.right)
          // nd.key = key; nd.value = value
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

// O(n) returns an in-order iterator starting from a node
export function* inOrder(nd, key = false) {
  if (nd) {
    yield* inOrder(nd.left, key)
    yield key ? nd.key : nd
    yield* inOrder(nd.right, key)
  }
}

// O(n) returns an pre-order iterator starting from a node
export function* preOrder(nd, key = false) {
  if (nd) {
    yield key ? nd.key : nd
    yield* preOrder(nd.left, key)
    yield* preOrder(nd.right, key)
  }
}

// O(n) returns an post-order iterator starting from a node
export function* postOrder(nd, key = false) {
  if (nd) {
    yield* postOrder(nd.left, key)
    yield* postOrder(nd.right, key)
    yield key ? nd.key : nd
  }
}

export class Trie {
  #root = new HTable()

  #leaf = HTable.from([["word", true]])

  get root() { return this.#root }

  [Symbol.iterator]() {
    return this.words()[Symbol.iterator]()
  }

  words(nd = this.#root) {
    const wds = []
    for (const [ch, next] of nd) {
      if (this.#leaf.equal(next)) { wds.push(ch); return wds }
      if (next.get("word")) { wds.push(ch) }
      const sufs = this.words(next)
      for (const suf of sufs) { wds.push(ch + suf) }
    }
    return wds
  }

  // O(key.length) inserts a key into a trie
  set(key) {
    let nd = this.#root
    for (const ch of key) {
      if (!nd.get(ch)) { nd.set(ch, new HTable()) }
      nd = nd.get(ch)
    }
    nd.set("word", true)
    return this
  }
}


// const htb = new HTable()
// htb.set("word", true)
// const leaf = HTable.from([["word", true]])
// console.log(htb, leaf)
// console.log(htb.equal(leaf))

// const trie = new Trie()
// trie.set("car").set("card").set("cat").set("cut")
// console.log(trie.words())
// console.log([...trie])
// console.log(trie.root)
// console.log(trie.root.get("c"))
// console.log(trie.root.get("c").get("a"))
// console.log(trie.root.get("c").get("a").get("r"))
// console.log(trie.root.get("c").get("a").get("r").get("d"))
