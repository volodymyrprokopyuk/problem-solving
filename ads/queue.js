import { inspect } from "util"
import { error } from "./util.js"
import { LNode } from "./list.js"

export class Queue {
  front = null
  rear = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const que = new Queue()
    for (const el of it) { que.enq(el) }
    return que
  }

  [Symbol.iterator]() {
    let nd = this.front
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

  [inspect.custom]() { return `Queue(${Array.from(this)})` }

  // O(1)
  enq(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.front = this.rear = nd }
    else { this.rear = this.rear.next = nd }
    ++this.#length
    return this
  }

  // O(1)
  deq() {
    if (this.#length === 0) { error("dequeue from empty queue") }
    const nd = this.front
    this.front = this.front.next
    --this.#length
    if (this.#length === 0) { this.front = this.rear = null }
    return nd.data
  }

  // O(1)
  peek() {
    if (this.#length === 0) { error("peek from empty queue") }
    return this.front.data
  }
}

// const que = new Queue()
// que.enq(1).enq(2).enq(3).enq(4)
const que = Queue.from([5, 6, 7, 8])
console.log(que, que.length)
while(que.length > 0) { console.log(que.deq()) }
que.enq(9).enq(10)
console.log(que.deq())
console.log(que.peek())
console.log(que, que.length)
