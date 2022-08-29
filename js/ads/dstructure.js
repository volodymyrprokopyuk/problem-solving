import { inOrder, preOrder, postOrder } from "./algorithm.js"

function error(msg) { throw new Error(msg) }

// ** LinkedList

class LNode {
  constructor (data) {
    this.data = data
    this.prev = this.next = null
  }
}

export class LList {
  constructor() {
    this.tail = this.head = null
    this.len = 0
  }

  // O(1)
  add(vl) {
    const nd = new LNode(vl)
    // Add the first element
    if (this.tail === null) { this.head = this.tail = nd }
    // Append to the tail
    else { this.tail = this.tail.next = nd }
    ++this.len
  }

  // O(1)
  addHead(vl) {
    const nd = new LNode(vl)
    // Add the first element
    if (this.head === null) { this.tail = this.head = nd }
    // Prepend to the head
    else {
      nd.next = this.head
      this.head = nd
    }
    ++this.len
  }

  // O(n)
  rem(vl) {
    // Element not found in the empty LList
    if (this.len === 0) { return false }
    // Found the head element
    if (vl === this.head.data) {
      // Remove the head singleton element
      if (this.head === this.tail) { this.tail = this.head = null }
      // Remothe the head element by forwarding the head
      else { this.head = this.head.next }
      --this.len
      return true
    }
    // Linear search
    let n = this.head
    while (n.next !== null) {
      // Found the element
      if (vl === n.next.data) {
        // Found the tail element. Backwards the tail
        if (n.next === this.tail) { this.tail = n }
        // Remove the found element by linking to the next element
        n.next = n.next.next
        --this.len
        return true
      }
      n = n.next
    }
    // Element not found
    return false
  }

  // O(n)
  find(vl) {
    let n = this.head
    // Linear search
    while (n !== null) {
      if (vl === n.data) { return true }
      n = n.next
    }
    return false
  }

  [Symbol.iterator]() {
    let n = this.head
    const next = () => {
      // Linear scan
      if (n === null) { return { done: true } }
      else {
        const value = n.data
        n = n.next
        return { value, done: false }
      }
    }
    return { next }
  }

  static from(it) {
    const ll = new LList()
    for (const el of it) { ll.add(el) }
    return ll
  }
}

const ll = LList.from([0])
console.log(Array.from(ll), ll.len)
ll.addHead(-1)
ll.add(1)
ll.add(2)
ll.add(3)
ll.addHead(-2)
console.log(ll.rem(-2), ll.rem(2), ll.rem(3), ll.rem(99))
ll.addHead(-2)
ll.add(2)
console.log(Array.from(ll), ll.len)
console.log(ll.find(1), ll.find(99))

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

// ** Queue

export class Queue {
  constructor() {
    this.arr = []
  }

  // O(1)
  get length() { return this.arr.length }

  // O(1) => O(n)
  enq(el) { this.arr.unshift(el) }

  // O(1)
  deq() {
    if (this.arr.length === 0) { error("deq from empty Queue") }
    return this.arr.pop()
  }

  // O(1)
  peek() {
    if (this.arr.length === 0) { error("peek from empty Queue") }
    return this.arr.at(-1)
  }

  // O(n)
  static from(it) {
    const qu = new Queue()
    for (const el of it) { qu.enq(el) }
    return qu
  }
}

// const qu = Queue.from([1, 2, 3, 4, 5])
// console.log(qu, qu.length)
// qu.enq(10)
// console.log(qu.deq(), qu.peek(), qu.deq(), qu, qu.length)

export class Queue2 {
  constructor() {
    this.tail = this.head = null
    this.length = 0
  }

  // O(1)
  enq(el) {
    const nd = new LNode(el)
    ++this.length
    if (this.tail === null) { this.head = this.tail = nd; return }
    this.tail = this.tail.next = nd
  }

  // O(1)
  deq() {
    if (this.length === 0) { error("deq from empty Queue2") }
    --this.length
    const n = this.head
    this.head = this.head.next
    if (this.head === null) { this.tail = null }
    return n.data
  }

  // O(1)
  peek() {
    if (this.length === 0) { error("peek from empty Queue2") }
    return this.head.data
  }

  // O(n)
  static from(it) {
    const qu = new Queue2()
    for (const el of it) { qu.enq(el) }
    return qu
  }

  [Symbol.iterator]() {
    const next = () => {
      if (this.length === 0) { return { value: undefined, done: true } }
      return { value: this.deq(), done: false }
    }
    return { next }
  }
}

// const qu = Queue2.from([1, 2, 3, 4, 5])
// // console.log(qu.length, Array.from(qu))
// qu.enq(10)
// console.log(qu.deq(), qu.peek(), qu.deq(), qu.length, Array.from(qu))

// TODO export class Deque { enq/deqHead/Tail }

// ** Tree

export class TNode {
  constructor(data) {
    this.data = data
    this.left = this.right = null
  }
}

export class BSTree {
  constructor() { this.root = null }

  #add(nd, vl) {
    if (vl < nd.data) {
      if (nd.left === null) { nd.left = new TNode(vl) }
      else { this.#add(nd.left, vl) }
    } else {
      if (nd.right === null) { nd.right = new TNode(vl) }
      else { this.#add(nd.right, vl) }
    }
  }

  #remove(nd, vl) {
    // TODO
  }

  #find(nd, vl) {
    if (nd === null) { return false }
    if (vl === nd.data) { return true }
    if (vl < nd.data) { return this.#find(nd.left, vl) }
    else { return this.#find(nd.right, vl) }
  }

  // O(log n)
  add(vl) {
    if (this.root === null) { this.root = new TNode(vl); return }
    this.#add(this.root, vl)
  }

  // O(log n)
  remove(vl) {
    if (this.root === null) { return }
    this.#remove(thir.root, vl)
  }

  // O(log n)
  find(vl) {
    if (this.root === null) { return false }
    return this.#find(this.root, vl)
  }

  static from(it) {
    const tr = new BSTree()
    for(const el of it) { tr.add(el) }
    return tr
  }
}

// const tr = BSTree.from([8, 1, 4, 3, 2, 6, 7, 9, 0, 5])
// inOrder(tr.root, console.log)
// console.log(tr.find(1), tr.find(-1))
