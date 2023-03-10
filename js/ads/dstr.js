/*
 * All = length O(1), from, iterator O(n)
 * * Array = [set], [get] O(1), includes, remove O(n)
 * * HTable (Array + hash) = set, get, remove O(1)
 * + Heap (arr + heapUp/Down) = push, pop O(log(n)), peek O(1)
 * - LNode = data, prev, next
 *   * List (head) = push, pop, peek O(1), includes, remove O(n)
 *   + Stack (top) = push, pop, peek O(1)
 *   + Queue (front, rear) = enqueue, dequeue, peek O(1)
 * - TNode = data, left, right
 *   * BSTree (root) = set, get, remove O(log(n))
 */

function error(message) { throw new Error(message) }

function equal(a, b) { return a === b }
function lt(a, b) { return a < b }
function gt(a, b) { return a > b }

function swap(arr, i, j) {
  const el = arr[i]
  arr[i] = arr[j]
  arr[j] = el
}

export class Heap {
  #cmp = lt
  #arr = []

  constructor(cmp) { this.#cmp = cmp }

  get length() { return this.#arr.length }

  static from(it, cmp = lt) {
    const hp = new Heap(cmp)
    for (const el of it) { hp.push(el) }
    return hp
  }

  [Symbol.iterator]() {
    const next = () => {
      if (this.#arr.length !== 0) {
        return { value: this.pop(), done: false }
      } else { return { done: true } }
    }
    return { next }
  }

  // O(log(n))
  push(data) {
    const heapUp = (i) => {
      const p = Math.floor((i - 1) / 2)
      if (this.#cmp(this.#arr[i], this.#arr[p])) {
        swap(this.#arr, i, p)
        heapUp(p)
      }
    }
    this.#arr.push(data)
    heapUp(this.#arr.length - 1)
  }

  // O(log(n))
  pop() {
    const child = (i) => {
      const l = i * 2 + 1, r = i * 2 + 2
      if (r <= this.#arr.length - 1) {
        return this.#cmp(this.#arr[l], this.#arr[r]) ? l : r
      } else if (l <= this.#arr.length - 1) {
        return l
      } else { return -1 }
    }
    const heapDown = (i) => {
      const c = child(i)
      if (c !== -1 && this.#cmp(this.#arr[c], this.#arr[i])) {
        swap(this.#arr, i, c)
        heapDown(c)
      }
    }
    if (this.#arr.length === 0) { error("pop from empty heap") }
    const data = this.#arr[0]
    this.#arr[0] = this.#arr.at(-1)
    --this.#arr.length
    heapDown(0)
    return data
  }

  // O(1)
  peek() {
    if (this.#arr.length === 0) { error("peeek from empty heap") }
    return this.#arr[0]
  }
}

class LNode {
  data = null
  prev = null
  next = null

  constructor(data) { this.data = data }
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
    const next = () => {
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
  includes(data, eq = equal) {
    let nd = this.#head
    while (nd !== null) {
      if (eq(nd.data, data)) { return true }
      nd = nd.next
    }
    return false
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
    const next = () => {
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

export class Queue {
  #front = null
  #rear = null
  length = 0

  static from(it) {
    const qu = new Queue()
    for (const el of it) { qu.enqueue(el) }
    return qu
  }

  [Symbol.iterator]() {
    let nd = this.#front
    const next = () => {
      if (nd !== null) {
        const value = nd.data
        nd = nd.next
        return { value, done: false }
      } else { return { done: true } }
    }
    return { next }
  }

  // O(1)
  enqueue(data) {
    const nd = new LNode(data)
    if (this.#front === null && this.#rear === null) {
      this.#front = this.#rear = nd
    } else {
      this.#rear = this.#rear.next = nd
    }
    ++this.length
  }

  // O(1)
  dequeue() {
    if (this.#front === null && this.#rear === null) {
      error("dequeue from empty queue")
    }
    const data = this.#front.data
    if (this.#front === this.#rear) {
      this.#front = this.#rear = null
    } else {
      this.#front = this.#front.next
    }
    --this.length
    return data
  }

  // O(1)
  peek() {
    if (this.#front === null && this.#rear === null) {
      error("dequeue from empty queue")
    }
    return this.#front.data
  }
}

class TNode {
  data = null
  left = null
  right = null

  constructor(data) { this.data = data }
}

export class BSTree {
  #cmp = lt
  #root = null
  length = 0

  constructor(cmp = lt) { this.#cmp = cmp }

  static from(it, cmp = lt) {
    const tr = new BSTree(cmp)
    for (const el of it) { tr.set(el) }
    return tr
  }

  get inOrder() {
    function* inOrderGen(nd) {
      if (nd !== null) {
        yield* inOrderGen(nd.left)
        yield nd.data
        yield* inOrderGen(nd.right)
      }
    }
    return { [Symbol.iterator]: () => inOrderGen(this.#root) }
  }

  // O(log(n))
  set(data) {
    const setNode = (nd, data) => {
      if (nd === null) {
        nd = new TNode(data)
        ++this.length
      } else if (this.#cmp(data, nd.data)) {
        nd.left = setNode(nd.left, data)
      } else {
        nd.right = setNode(nd.right, data)
      }
      return nd
    }
    this.#root = setNode(this.#root, data)
  }

  // O(log(n))
  get(data, eq = equal) {
    const getNode = (nd) => {
      if (nd !== null) {
        if (eq(nd.data, data)) { return nd.data }
        if (this.#cmp(data, nd.data)) {
          return getNode(nd.left)
        } else { return getNode(nd.right) }
      }
    }
    return getNode(this.#root)
  }

  // O(log(n))
  remove(data, eq = equal) {
    const inOrderSucc = (nd) => {
      while (nd.left !== null) { nd = nd.left }
      return nd
    }
    const removeNode = (nd, data) => {
      if (nd === null) { return null }
      if (this.#cmp(data, nd.data)) {
        nd.left = removeNode(nd.left, data)
      } else if (this.#cmp(nd.data, data)) {
        nd.right = removeNode(nd.right, data)
      } else {
        if (nd.left === null) {
          --this.length
          return nd.right
        } else if (nd.right === null) {
          --this.length
          return nd.left
        } else {
          // The in-order successor becomes the new root
          nd.data = inOrderSucc(nd.right).data
          // Delete the in-order successor
          nd.right = removeNode(nd.right, nd.data)
        }
      }
      return nd
    }
    const length = this.length
    this.#root = removeNode(this.#root, data)
    return length > this.length
  }
}

// const hp = Heap.from([5, 2, 3, 4, 1], gt)
// for (const el of hp) { console.log(el) }

// const ls = List.from([1, 2, 3, 4])
// for (const el of ls) { console.log(el) }

// const st = Stack.from([1, 2, 3, 4])
// for (const el of st) { console.log(el) }

// const qu = Queue.from([1, 2, 3, 4])
// for (const el of qu) { console.log(el) }

const tr = BSTree.from([5, 2, 4, 3, 1])
for (const el of tr.inOrder) { console.log(el) }
