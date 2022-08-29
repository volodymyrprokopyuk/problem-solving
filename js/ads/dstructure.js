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
    this.length = 0
  }

  // O(1)
  add(vl) {
    const nd = new LNode(vl)
    // Add the first element
    if (this.tail === null) { this.head = this.tail = nd }
    // Append to the tail
    else { this.tail = this.tail.next = nd }
    ++this.length
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
    ++this.length
  }

  // O(n)
  rem(vl) {
    // Element not found in the empty LList
    if (this.length === 0) { return false }
    // Found the head element
    if (vl === this.head.data) {
      // Remove the head singleton element and make the LList empty
      if (this.head === this.tail) { this.tail = this.head = null }
      // Remothe the head element and forward the head
      else { this.head = this.head.next }
      --this.length
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
        --this.length
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

// const ll = LList.from([0])
// console.log(Array.from(ll), ll.length)
// ll.addHead(-1)
// ll.add(1)
// ll.add(2)
// ll.add(3)
// ll.addHead(-2)
// console.log(ll.rem(-2), ll.rem(2), ll.rem(3), ll.rem(99))
// ll.addHead(-2)
// ll.add(2)
// console.log(Array.from(ll), ll.length)
// console.log(ll.find(1), ll.find(99))

// TODO export class DList {  } Doubly linked list
// TODO export class CList {  } Circular linked list

// ** Stack

export class Stack {
  constructor() {
    this.head = null
    this.length = 0
  }

  // O(1)
  push(vl) {
    const nd = new LNode(vl)
    // Add the first element
    if (this.head === null) { this.head = nd }
    // Prepend to the head
    else {
      nd.next = this.head
      this.head = nd
    }
    ++this.length
  }

  // O(1)
  pop() {
    if (this.length === 0) { error("pop from empty Stack") }
    // Return the head value and forward the head
    const vl = this.head.data
    this.head = this.head.next
    --this.length
    return vl
  }

  // O(1)
  peek() {
    if (this.length === 0) { error("peek from empty Stack") }
    // Return the top value
    return this.head.data
  }

  static from(it) {
    const st = new Stack()
    for(const el of it) { st.push(el) }
    return st
  }
}

// const st = Stack.from([0])
// console.log(st, st.length)
// st.push(1)
// st.push(2)
// console.log(st.pop(), st.peek(), st.pop())
// console.log(st, st.length)

// ** Queue

export class Queue {
  constructor() {
    this.tail = this.head = null
    this.length = 0
  }

  // O(1)
  enq(vl) {
    const nd = new LNode(vl)
    // Enqueue the firs element
    if (this.tail === null) { this.head = this.tail = nd }
    // Append to the tail
    else { this.tail = this.tail.next = nd }
    ++this.length
  }

  // O(1)
  deq() {
    if (this.length === 0) { error("deq from empty Queue") }
    // Return the head value
    const vl = this.head.data
    // Dequeue the head singleton element and make the Queue empty
    if (this.head === this.tail) { this.tail = this.head = null }
    // Dequeue the head element and forward the head
    else { this.head = this.head.next }
    --this.length
    return vl
  }

  // O(1)
  peek() {
    if (this.length === 0) { error("deq from empty Queue") }
    // Return the head value
    return this.head.data
  }

  static from(it) {
    const qu = new Queue()
    for (const el of it) { qu.enq(el) }
    return qu
  }
}

// const qu = Queue.from([0])
// qu.enq(1)
// qu.enq(2)
// console.log(qu.deq(), qu.peek(), qu.deq())
// console.log(qu, qu.length)

// TODO export class Deque { enq, deq, enqHead, deqTail }

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
