/*
 * All = length O(1), from, iterator O(n)
 * Array = [set], [get] O(1), remove O(n)
 * HTable (Array + hash) = set, get, remove O(1)
 * Heap (Array + heapify) = push, pop O(log n), peek O(1)
 * + LNode = data, prev, next
 *   + List (head) = push, pop, peek O(1), remove O(n)
 *   + Stack (top) = push, pop, peek O(1)
 *   Queue (front, rear) = enqueue, dequeue, peek O(1)
 * TNode = data, left, right
 *   BSTree (root) = set, get, remove O(log n)
 */

function error(message) { throw new Error(message) }

function equal(a, b) { return a === b }

class LNode {
  constructor(data) {
    this.data = data
    this.prev = this.next = null
  }
}

export class List {
  #head = null
  length = 0

  static from(it) {
    const ls = new List()
    for (const data of it) { ls.push(data) }
    return ls
  }

  [Symbol.iterator]() {
    let nd = this.#head
    function next() {
      if (nd !== null) {
        const value = nd.data
        nd = nd.next
        return { value, done: false }
      } else { return { done: true } }
    }
    return { next }
  }

  // O(1)
  push(data) {
    const nd = new LNode(data)
    nd.next = this.#head
    this.#head = nd
    ++this.length
  }

  // O(1)
  pop() {
    if (this.#head === null) { error("pop from empty list") }
    const data = this.#head.data
    this.#head = this.#head.next
    --this.length
    return data
  }

  // O(1)
  peek() {
    if (this.#head === null) { error("peek from empty list") }
    return this.#head.data
  }

  // O(n)
  remove(data, eq = equal) {
    if (this.#head === null) { error("remove from empty list") }
    let nd = this.#head
    if (eq(nd.data, data)) {
      this.#head = nd.next
      return true
    }
    while (nd.next !== null) {
      if (eq(nd.next.data, data)) {
        nd.next = nd.next.next
        --this.length
        return true
      }
      nd = nd.next
    }
    return false
  }
}

export class Stack {
  #top = null
  length = 0

  static from(it) {
    const st = new Stack()
    for (const el of it) { st.push(el) }
    return st
  }

  [Symbol.iterator]() {
    let nd = this.#top
    function next() {
      if (nd !== null) {
        const value = nd.data
        nd = nd.next
        return { value, done: false }
      } else { return { done: true } }
    }
    return { next }
  }

  // O(1)
  push(data) {
    const nd = new LNode(data)
    nd.next = this.#top
    this.#top = nd
    ++this.length
  }

  // O(1)
  pop() {
    if (this.#top === null) { error("pop from empty stack") }
    const data = this.#top.data
    this.#top = this.#top.next
    --this.length
    return data
  }

  // O(1)
  peek() {
    if (this.#top === null) { error("pop from empty stack") }
    return this.#top.data
  }
}

// const ls = List.from([1, 2, 3, 4])
// for (const el of ls) { console.log(el) }

const st = Stack.from([1, 2, 3, 4])
for (const el of st) { console.log(el) }
console.log(st.pop(), st)
console.log(st.pop(), st)
console.log(st.peek(), st)
