import { inspect } from "util"
import { error } from "./util.js"
import { LNode } from "./list.js"

export class Stack {
  top = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const stk = new Stack()
    for (const el of it) { stk.push(el) }
    return stk
  }

  [Symbol.iterator]() {
    let nd = this.top
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

  [inspect.custom]() { return `Stack(${Array.from(this)})` }

  // O(1)
  push(data) {
    const nd = new LNode(data)
    nd.next = this.top
    this.top = nd
    ++this.#length
    return this
  }

  // O(1)
  pop() {
    if (this.#length === 0) { error("pop from empty stack") }
    const value = this.top.data
    this.top = this.top.next
    --this.#length
    return value
  }

  peek() {
    if (this.#length === 0) { error("pop from empty stack") }
    return this.top.data
  }
}

const stk = Stack.from([1, 2, 3, 4, 5])
console.log(stk.peek(), stk.pop())
console.log(stk)
