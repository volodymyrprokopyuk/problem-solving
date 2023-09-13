import { inspect } from "util"
import { error } from "./util.js"
import { LNode } from "./list.js"

export class Queue {
  #front = null; #rear = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const que = new Queue()
    for (const el of it) { que.enq(el) }
    return que
  }

  [Symbol.iterator]() {
    function* values(nd) {
      while (nd) { yield nd.data; nd = nd.next }
    }
    return values(this.#front)
  }

  [inspect.custom]() { return `Queue(${[...this]})` }

  // O(1) enqueues an element to a rear
  enq(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.#front = this.#rear = nd }
    else { this.#rear = this.#rear.next = nd }
    ++this.#length
    return this
  }

  // O(1) dequeues an element from a front
  deq() {
    if (this.#length === 0) { error("deq from empty queue") }
    const nd = this.#front
    --this.#length
    if (this.#length === 0) { this.#front = this.#rear = null }
    else { this.#front = this.#front.next }
    return nd.data
  }

  // O(1) peeks an element from a front
  peek() {
    if (this.#length === 0) { error("peek from empty queue") }
    return this.#front.data
  }
}

export class Deque {
  #front = null; #rear = null
  #length = 0

  get length() { return this.#length }

  static from(it) {
    const deq = new Deque()
    for (const el of it) { deq.enq(el) }
    return deq
  }

  [Symbol.iterator]() {
    function* values(nd) {
      while (nd) { yield nd.data; nd = nd.next }
    }
    return values(this.#front)
  }

  [inspect.custom]() { return `Deque(${[...this]})` }

  // O(1) enqueues an element to a rear
  enq(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.#front = this.#rear = nd }
    else { nd.prev = this.#rear; this.#rear = this.#rear.next = nd }
    ++this.#length
    return this
  }

  // O(1) enqueues an element to a front
  enqFront(data) {
    const nd = new LNode(data)
    if (this.#length === 0) { this.#front = this.#rear = nd }
    else { nd.next = this.#front; this.#front = this.#front.prev = nd }
    ++this.#length
    return this
  }

  // O(1) dequeues an element from a front
  deq() {
    if (this.#length === 0) { error("deq from empty deque") }
    const nd = this.#front
    --this.#length
    if (this.#length === 0) { this.#front = this.#rear = null }
    else { this.#front = this.#front.next; this.#front.prev = null }
    return nd.data
  }

  // O(1) dequeues an element from a rear
  deqRear() {
    if (this.#length === 0) { error("deqRear from empty deque") }
    const nd = this.#rear
    --this.#length
    if (this.#length === 0) { this.#front = this.#rear = null }
    else { this.#rear = this.#rear.prev; this.#rear.next = null }
    return nd.data
  }

  // O(1) peeks an element from a front
  peek() {
    if (this.#length === 0) { error("peek from empty deque") }
    return this.#front.data
  }

  // O(1) peeks an element from a rear
  peekRear() {
    if (this.#length === 0) { error("peekRear from empty deque") }
    return this.#rear.data
  }
}
