import { inspect } from "util"
import { error } from "./util.js"

export class LNode {
  value
  prev = null; next = null

  constructor(value) { this.value = value }
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

  [Symbol.iterator]() { return this.values() }

  entries() {
    function* entries(nd) {
      while (nd) { yield nd; nd = nd.next }
    }
    return entries(this.#head)
  }

  values() {
    function* values(nd) {
      while (nd) { yield nd.value; nd = nd.next }
    }
    return values(this.#head)
  }

  [inspect.custom](path, opts) { return `List(${[...this].join(", ")})` }

  // O(1) pushes an element to a head
  push(value) {
    const nd = new LNode(value)
    nd.next = this.#head
    this.#head = nd
    ++this.#length
    return this
  }

  // O(1) pops an element from a head
  pop() {
    if (this.#length === 0) { error("pop from empty list") }
    const nd = this.#head
    this.#head = this.#head.next
    --this.#length
    return nd.value
  }

  // O(1) peeks an element from a head
  peek() {
    if (this.#length === 0) { error("peek from empty list") }
    return this.#head.value
  }

  // O(n) deletes a matching element or undefined
  delete(value, eq = (a, b) => a === b) {
    let nd = this.#head
    if (nd) {
      if (eq(value, nd.value)) {
        this.#head = nd.next
        --this.#length
        return nd.value
      }
      while (nd.next) {
        if (eq(value, nd.next.value)) {
          const value = nd.next.value
          nd.next = nd.next.next
          --this.#length
          return value
        }
        nd = nd.next
      }
    }
  }

  // O(n) returns a matching element or undefined
  get(value, eq = (a, b) => a === b) {
    for (const nd of this.entries()) {
      if (eq(value, nd.value)) { return nd }
    }
  }

  // O(n) reverses a list in place
  reverse() {
    let prev = null, curr = this.#head, next
    while(curr) {
      next = curr.next
      curr.next = prev; prev = curr
      curr = next
    }
    this.#head = prev
  }
}

export class DList {
  #head = null; #tail = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const lst = new DList()
    for (const el of it) { lst.pushTail(el) }
    return lst
  }

  [Symbol.iterator]() { return this.values() }

  entries(reverse = false) {
    function* entries(nd) {
      while (nd) { yield nd; nd = reverse ? nd.prev : nd.next }
    }
    return entries(reverse ? this.#tail : this.#head)
  }

  values(reverse = false) {
    function* values(nd) {
      while (nd) { yield nd.value; nd = reverse ? nd.prev : nd.next }
    }
    return values(reverse ? this.#tail : this.#head)
  }

  [inspect.custom]() { return `DList(${[...this].join(", ")})` }

  // O(1) pushes an element to a head
  push(value) {
    const nd = new LNode(value)
    if (this.#length === 0) { this.#head = this.#tail = nd }
    else { nd.next = this.#head; this.#head = this.#head.prev = nd }
    ++this.#length
    return this
  }

  // O(1) pushes an element to a tail
  pushTail(value) {
    const nd = new LNode(value)
    if (this.#length === 0) { this.#head = this.#tail = nd }
    else { nd.prev = this.#tail; this.#tail = this.#tail.next = nd }
    ++this.#length
    return this
  }

  // O(1) pops an element from a head
  pop() {
    if (this.#length === 0) { error("pop from empty dlist") }
    const nd = this.#head
    --this.#length
    if (this.#length === 0) { this.#head = this.#tail = null }
    else { this.#head = this.#head.next; this.#head.prev = null }
    return nd.value
  }

  // O(1) pops an element from a tail
  popTail() {
    if (this.#length === 0) { error("popTail from empty dlist") }
    const nd = this.#tail
    --this.#length
    if (this.length === 0) { this.#head = this.#tail = null }
    else { this.#tail = this.#tail.prev; this.#tail.next = null }
    return nd.value
  }

  // O(1) peeks an element from a head
  peek() {
    if (this.#length === 0) { error("peek from empty dlist") }
    return this.#head.value
  }

  // O(1) peeks an element from a tail
  peekTail() {
    if (this.#length === 0) { error("peekTail from empty dlist") }
    return this.#tail.value
  }

  // O(1) inserts an element after a node
  insert(node, value) {
    const nd = new LNode(value), next = node.next
    node.next = nd; nd.prev = node
    if (next) { nd.next = next; next.prev = nd }
    else { this.#tail = nd }
    ++this.#length
  }

  // O(1) deletes a node
  delete(node) {
    --this.#length
    if (this.#length === 0) { this.#head = this.#tail = null }
    else if (node === this.#head) {
      this.#head = this.#head.next; this.#head.prev = null
    } else if (node === this.#tail) {
      this.#tail = this.#tail.prev; this.#tail.next = null
    } else { node.prev.next = node.next; node.next.prev = node.prev }
  }

  // O(n) returns a matching element or undefined
  get(value, eq = (a, b) => a === b) {
    for (const nd of this.entries()) {
      if (eq(value, nd.value)) { return nd }
    }
  }
}
