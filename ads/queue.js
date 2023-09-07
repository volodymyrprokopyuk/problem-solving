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
    if (this.#length === 0) { error("deq from empty queue") }
    const nd = this.front
    --this.#length
    if (this.#length === 0) { this.front = this.rear = null }
    else { this.front = this.front.next }
    return nd.data
  }

  // O(1)
  peek() {
    if (this.#length === 0) { error("peek from empty queue") }
    return this.front.data
  }
}

export class Deque {
  front = null
  rear = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const deq = new Deque()
    for (const el of it) { deq.enq(el) }
    return deq
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

  [inspect.custom]() { return `Deque(${Array.from(this)})` }

  // O(1)
  enq(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.front = this.rear = nd }
    else { nd.prev = this.rear; this.rear = this.rear.next = nd }
    ++this.#length
    return this
  }

  // O(1)
  enqFront(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.front = this.rear = nd }
    else { nd.next = this.front; this.front = this.front.prev = nd }
    ++this.#length
    return this
  }

  // O(1)
  deq() {
    if (this.#length === 0) { error("deq from empty deque") }
    const nd = this.front
    --this.#length
    if (this.#length === 0) { this.front = this.rear = null }
    else { this.front = this.front.next; this.front.prev = null }
    return nd.data
  }

  // O(1)
  deqRear() {
    if (this.#length === 0) { error("deqRear from empty deque") }
    const nd = this.rear
    --this.#length
    if (this.#length === 0) { this.front = this.rear = null }
    else { this.rear = this.rear.prev; this.rear.next = null }
    return nd.data
  }

  // O(1)
  peek() {
    if (this.#length === 0) { error("peek from empty deque") }
    return this.front.data
  }

  // O(1)
  peekRear() {
    if (this.#length === 0) { error("peekRear from empty deque") }
    return this.rear.data
  }
}
