import { inspect } from "util"
import { error } from "./util.js"

export class LNode {
  data
  prev = null; next = null

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

  [Symbol.iterator]() { return this.nodes(true) }

  nodes(value = false) {
    function* nodes(nd) {
      while (nd) {
        yield value ? nd.data : nd
        nd = nd.next
      }
    }
    return nodes(this.#head)
  }

  [inspect.custom](path, opts) { return `List(${[...this]})` }

  // O(1) pushes an element to a head
  push(data) {
    const nd = new LNode(data)
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
    return nd.data
  }

  // O(1) peeks an element from a head
  peek() {
    if (this.#length === 0) { error("peek from empty list") }
    return this.#head.data
  }

  // O(n) returns a matching element or undefined
  get(data, eq = (a, b) => a === b) {
    for (const nd of this.nodes()) {
      if (eq(data, nd.data)) { return nd }
    }
  }

  // O(n) deletes a matching element or undefined
  delete(data, eq = (a, b) => a === b) {
    let nd = this.#head
    if (nd) {
      if (eq(data, nd.data)) {
        this.#head = nd.next
        --this.#length
        return nd.data
      }
      while (nd.next) {
        if (eq(data, nd.next.data)) {
          const data = nd.next.data
          nd.next = nd.next.next
          --this.#length
          return data
        }
        nd = nd.next
      }
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

  [Symbol.iterator]() { return this.nodes(true) }

  nodes(value = false) {
    function* nodes(nd) {
      while (nd) {
        yield value ? nd.data : nd
        nd = nd.next
      }
    }
    return nodes(this.#head)
  }

  reverse(value = true) {
    function* nodes(nd) {
      while (nd) {
        yield value ? nd.data : nd
        nd = nd.prev
      }
    }
    return nodes(this.#tail)
  }

  [inspect.custom]() { return `DList(${[...this]})` }

  // O(1) pushes an element to a head
  push(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.#head = this.#tail = nd }
    else { nd.next = this.#head; this.#head = this.#head.prev = nd }
    ++this.#length
    return this
  }

  // O(1) pushes an element to a tail
  pushTail(data) {
    const nd = new LNode(data)
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
    return nd.data
  }

  // O(1) pops an element from a tail
  popTail() {
    if (this.#length === 0) { error("popTail from empty dlist") }
    const nd = this.#tail
    --this.#length
    if (this.length === 0) { this.#head = this.#tail = null }
    else { this.#tail = this.#tail.prev; this.#tail.next = null }
    return nd.data
  }

  // O(1) peeks an element from a head
  peek() {
    if (this.#length === 0) { error("peek from empty dlist") }
    return this.#head.data
  }

  // O(1) peeks an element from a tail
  peekTail() {
    if (this.#length === 0) { error("peekTail from empty dlist") }
    return this.#tail.data
  }

  // O(n) returns a matching element or undefined
  get(data, eq = (a, b) => a === b) {
    for (const nd of this.nodes()) {
      if (eq(data, nd.data)) { return nd }
    }
  }

  // O(1) inserts an element after a node
  insert(node, data) {
    const nd = new LNode(data), next = node.next
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
}
