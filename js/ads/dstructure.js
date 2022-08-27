function error(msg) { throw new Error(msg) }

// ** LinkedList

class LNode {
  constructor (data) {
    this.data = data
    this.next = null
    this.prev = null
  }
}

// Preserves the append ordering (by appending to the tail)
export class LList {
  constructor() {
    this.tail = this.head = null
    this.length = 0
  }

  // O(1)
  append(vl) {
    const nd = new LNode(vl)
    ++this.length
    if (this.head === null) { this.tail = this.head = nd; return }
    this.tail = this.tail.next = nd
  }

  // O(n)
  remove(vl) {
    let n = this.head
    if (n === null) { error("remove from empty LList") }
    if (n.data === vl) {
      --this.length
      this.head = this.head.next
      return
    }
    while (n.next !== null) {
      if (n.next.data === vl) {
        if (n.next === this.tail) { this.tail = n }
        --this.length
        n.next = n.next.next
        return
      }
      n = n.next
    }
  }

  // O(n)
  static from(it) {
    const ll = new LList()
    for (const el of it) { ll.append(el) }
    return ll
  }

  // O(n)
  [Symbol.iterator]() {
    let n = this.head
    const next = () => {
      if (n === null) { return { value: undefined, done: true } }
      const value = n.data
      n = n.next
      return { value, done: false }
    }
    return { next }
  }
}

// const ll = LList.from([1, 2, 3, 4, 5])
// console.log(Array.from(ll), ll.length)
// ll.remove(1)
// ll.remove(3)
// ll.remove(5)
// ll.remove(99)
// ll.append(10)
// console.log(Array.from(ll), ll.length)

// Reverse the prepend ordering (by prepending to the head)
export class RList {
  constructor() {
    this.head = null
    this.length = 0
  }

  // O(1)
  prepend(vl) {
    const nd = new LNode(vl)
    ++this.length
    if (this.head === null) { this.head = nd; return }
    nd.next = this.head
    this.head = nd
  }

  // O(1)
  pop() {
    if (this.head === null) { error("pop from empty RList") }
    --this.length
    const n = this.head
    this.head = this.head.next
    return n.data
  }

  // O(1)
  peek() {
    if (this.head === null) { error("peek from empty RList") }
    return this.head.data
  }

  // O(n)
  static from(it) {
    const rl = new RList()
    for (const el of it) { rl.prepend(el) }
    return rl
  }

  // O(n)
  [Symbol.iterator]() {
    let n = this.head
    const next = () => {
      if (n === null) { return { value: undefined, done: true } }
      const value = n.data
      n = n.next
      return { value, done: false }
    }
    return { next }
  }
}

// const rl = RList.from([1, 2, 3, 4, 5])
// console.log(Array.from(rl), rl.length)
// console.log(rl.pop(), rl.pop())
// rl.prepend(10)
// console.log(Array.from(rl), rl.length)

// TODO export class DList {  } Doubly linked list
// TODO export class CList {  } Circular linked list

// ** Stack

export class Stack {
  constructor() { this.arr = [] }

  // O(1)
  get length() { return this.arr.length }

  // O(1)
  push(el) { this.arr.push(el) }

  // O(1)
  pop() {
    if (this.length === 0) { error("pop from empty Stack") }
    return this.arr.pop()
  }

  // O(1)
  peek() {
    if (this.length === 0) { error("peek from empty Stack") }
    return this.arr.at(-1)
  }

  // O(n)
  static from(it) {
    const st = new Stack()
    for (const el of it) { st.push(el) }
    return st
  }

  // O(n)
  [Symbol.iterator]() {
    const next = () => {
      if (this.length === 0) { return { value: undefined, done: true } }
      return { value: this.pop(), done: false }
    }
    return { next }
  }
}

// const st = Stack.from([1, 2, 3, 4, 5])
// console.log(st, st.length)
// console.log(st.pop(), st.peek(), st.pop(), st, st.length)

export class Stack2 {
  constructor() {
    this.rl = new RList()
  }

  // O(1)
  get length() { return this.rl.length }

  // O(1)
  push(el) { this.rl.prepend(el) }

  // O(1)
  pop() { return this.rl.pop() }

  // O(1)
  peek() { return this.rl.peek() }

  // O(n)
  static from(it) {
    const st = new Stack2()
    for (const el of it) { st.push(el) }
    return st
  }

  // O(n)
  [Symbol.iterator]() {
    const next = () => {
      if (this.length === 0) { return { value: undefined, done: true } }
      return { value: this.pop(), done: false }
    }
    return { next }
  }
}

// const st = Stack2.from([1, 2, 3, 4, 5])
// console.log(st.pop(), st.peek(), st.pop(), st.length, Array.from(st))
