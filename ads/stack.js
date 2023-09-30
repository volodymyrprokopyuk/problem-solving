import { inspect } from "util"
import { error } from "./util.js"
import { LNode } from "./list.js"

export class Stack {
  #top = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const stk = new Stack()
    for (const el of it) { stk.push(el) }
    return stk
  }

  [Symbol.iterator]() { return this.values() }

  values() {
    function* values(nd) {
      while (nd) { yield nd.value; nd = nd.next }
    }
    return values(this.#top)
  }

  [inspect.custom]() { return `Stack(${[...this].join(", ")})` }

  // O(1) pushes an element on a top
  push(value) {
    const nd = new LNode(value)
    nd.next = this.#top
    this.#top = nd
    ++this.#length
    return this
  }

  // O(1) pops an element from a top
  pop() {
    if (this.#length === 0) { error("pop from empty stack") }
    const nd = this.#top
    this.#top = this.#top.next
    --this.#length
    return nd.value
  }

  // O(1) peeks an element from a top
  peek() {
    if (this.#length === 0) { error("peek from empty stack") }
    return this.#top.value
  }
}
