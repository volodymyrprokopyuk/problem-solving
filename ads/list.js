import { inspect } from "util"
import { error } from "./util.js"

export class LNode {
  data; prev = null; next = null

  constructor(data) { this.data = data }
}

export class List {
  #head = null
  #length = 0

  get length() { return this.#length }

  static from (it) {
    const lst = new List()
    for (const el of it) { lst.push(el) }
    return lst
  }

  [Symbol.iterator]() {
    let nd = this.#head
    const next = () => {
      if (nd) {
        const value = nd.data
        nd = nd.next
        return { value, done: false }
      }
      return { done: true }
    }
    return { next }
  }

  toString() { return `List(${Array.from(this)})`}

  [inspect.custom](path, opts) { return this.toString() }

  // O(1)
  push(data) {
    const nd = new LNode(data)
    nd.next = this.#head
    this.#head = nd
    ++this.#length
    return this
  }

  // O(1)
  pop() {
    if (this.#length === 0) { error("pop from empty List") }
    const value = this.#head.data
    this.#head = this.#head.next
    --this.#length
    return value
  }

  // O(1)
  peek() {
    if (this.#length === 0) { error("peek from empty List") }
    return this.#head.data
  }

  // O(n)
  has(data, eq = (a, b) => a === b) {
    let nd = this.#head
    while (nd) {
      if (eq(nd.data, data)) { return true }
      nd = nd.next
    }
    return false
  }

  // O(n)
  remove(data, eq = (a, b) => a === b) {
    if (this.#length === 0) { error("remove from empty List") }
    let nd = this.#head
    if (eq(nd.data, data)) {
      this.#head = nd.next
      --this.#length
      return true
    }
    while (nd.next) {
      if (eq(nd.next.data, data)) {
        nd.next = nd.next.next
        --this.#length
        return true
      }
      nd = nd.next
    }
    return false
  }
}
