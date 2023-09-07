import { inspect } from "util"
import { error } from "./util.js"

export class LNode {
  data; prev = null; next = null

  constructor(data) { this.data = data }
}

export class List {
  head = null
  #length = 0

  get length() { return this.#length }

  static from (it) {
    const lst = new List()
    for (const el of it) { lst.push(el) }
    return lst
  }

  [Symbol.iterator]() {
    let nd = this.head
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

  [inspect.custom](path, opts) { return `List(${Array.from(this)})` }

  // O(1)
  push(data) {
    const nd = new LNode(data)
    nd.next = this.head
    this.head = nd
    ++this.#length
    return this
  }

  // O(1)
  pop() {
    if (this.#length === 0) { error("pop from empty list") }
    const nd = this.head
    this.head = this.head.next
    --this.#length
    return nd.data
  }

  // O(1)
  peek() {
    if (this.#length === 0) { error("peek from empty list") }
    return this.head.data
  }

  // O(n)
  get(data, eq = (a, b) => a === b) {
    let nd = this.head
    while (nd) {
      if (eq(nd.data, data)) { return nd.data }
      nd = nd.next
    }
  }

  // O(n)
  delete(data, eq = (a, b) => a === b) {
    let nd = this.head
    if (nd) {
      if (eq(nd.data, data)) {
        this.head = nd.next
        --this.#length
        return nd.data
      }
      while (nd.next) {
        if (eq(nd.next.data, data)) {
          const data = nd.next.data
          nd.next = nd.next.next
          --this.#length
          return data
        }
        nd = nd.next
      }
    }
  }

  // O(n)
  reverse() {
    let prev = null, curr = this.head, next
    while(curr) {
      next = curr.next
      curr.next = prev; prev = curr
      curr = next
    }
    this.head = prev
  }
}

// // const lst = new List()
// const lst = List.from([1, 2, 3, 4, 5])
// console.log(lst)
// console.log(lst.delete(5))
