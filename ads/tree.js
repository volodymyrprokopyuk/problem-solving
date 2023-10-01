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

  static from(it) {
    const trie = new Trie()
    for (const el of it) { trie.set(el) }
    return trie
  }

  [Symbol.iterator]() { return this.keys()[Symbol.iterator]() }

  [inspect.custom]() { return `Trie(${[...this].join(", ")})` }

  keys(nd = this.#root) { // complete words
    const wds = []
    for (const [ch, next] of nd) {
      if (next.length === 0) { return wds }
      if (next.length === 1 && next.get("word")) { wds.push(ch); return wds }
      if (next.get("word")) { wds.push(ch) }
      const suffs = this.keys(next)
      for (const suff of suffs) { wds.push(ch + suff) }
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

  // O(key.length) returns if a key is in a trie as a prefix or a complete word
  get(key, word = false) {
    let nd = this.#root
    for (const ch of key) {
      if (!nd.get(ch)) { return }
      nd = nd.get(ch)
    }
    return word ? nd.get("word") : true
  }

  // O(key.length) deletes a wrod from a trie
  delete(key) {
    let nd = this.#root
    for (const ch of key) {
      if (!nd.get(ch)) { return }
      nd = nd.get(ch)
    }
    return nd.delete("word") && key
  }
}
