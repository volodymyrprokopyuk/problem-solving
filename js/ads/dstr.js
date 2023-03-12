/*
 * All = length O(1), from, iterator O(n)
 * = Array = [set], [get] O(1), includes, remove O(n)
 *   = HTable (arr + hash) = set, get, remove O(1)
 *   + Heap (arr + heapUp/heapDown) = push, pop O(log(n)), peek O(1)
 * > LNode = data, prev, next
 *   * List (head) = push, pop, peek O(1), includes, remove O(n)
 *   + Stack (top) = push, pop, peek O(1)
 *   + Queue (front, rear) = enqueue, dequeue, peek O(1)
 * > TNode = data, left, right
 *   + BSTree (root) = set, get, remove O(log(n))
 * > GNode = name, weigth, adjs
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

  constructor(cmp = lt) { this.#cmp = cmp }

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
    const heapUp = (c) => {
      let p = Math.floor((c - 1) / 2)
      while (this.#cmp(this.#arr[c], this.#arr[p])) {
        swap(this.#arr, c, p)
        c = p
        p = Math.floor((c - 1) / 2)
      }
    }
    this.#arr.push(data)
    heapUp(this.#arr.length - 1)
  }

  // O(log(n))
  pop() {
    const child = (p) => {
      const l = p * 2 + 1, r = p * 2 + 2
      if (r < this.#arr.length) {
        return this.#cmp(this.#arr[l], this.#arr[r]) ? l : r
      } else if (l < this.#arr.length) {
        return l
      } else { return -1 }
    }
    const heapDown = (p) => {
      let c = child(p)
      while (c !== -1 && this.#cmp(this.#arr[c], this.#arr[p])) {
        swap(this.#arr, c, p)
        p = c
        c = child(p)
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
    if (this.#arr.length === 0) { error("peek from empty heap") }
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
    for (const el of it) { ls.push(el) }
    return ls
  }

  [Symbol.iterator]() {
    let nd = this.#head
    const next = () => {
      if (nd) {
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
    if (!this.#head) { error("pop from empty list") }
    const data = this.#head.data
    this.#head = this.#head.next
    --this.length
    return data
  }

  // O(1)
  peek() {
    if (!this.#head) { error("peek from empty list") }
    return this.#head.data
  }

  // O(n)
  includes(data, eq = equal) {
    let nd = this.#head
    while (nd) {
      if (eq(nd.data, data)) { return true }
      nd = nd.next
    }
    return false
  }

  // O(n)
  remove(data, eq = equal) {
    if (!this.#head) { error("remove from empty list") }
    let nd = this.#head
    if (eq(nd.data, data)) {
      this.#head = nd.next
      --this.length
      return true
    }
    while (nd.next) {
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
      if (nd) {
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
    if (!this.#top) { error("pop from empty stack") }
    const data = this.#top.data
    this.#top = this.#top.next
    --this.length
    return data
  }

  // O(1)
  peek() {
    if (!this.#top) { error("peek from empty stack") }
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
      if (nd) {
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
    if (!this.#front && !this.#rear) {
      this.#front = this.#rear = nd
    } else {
      this.#rear = this.#rear.next = nd
    }
    ++this.length
  }

  // O(1)
  dequeue() {
    if (!this.#front && !this.#rear) { error("dequeue from empty queue") }
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
    if (!this.#front && !this.#rear) { error("peek from empty queue") }
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
      if (nd) {
        yield* inOrderGen(nd.left)
        yield nd.data
        yield* inOrderGen(nd.right)
      }
    }
    return { [Symbol.iterator]: () => inOrderGen(this.#root) }
  }

  // O(log(n))
  set(data) {
    const newNd = new TNode(data)
    ++this.length
    if (!this.#root) { this.#root = newNd; return }
    let nd = this.#root, p
    while (nd) {
      p = nd
      if (this.#cmp(data, nd.data)) { nd = nd.left }
      else { nd = nd.right }
    }
    if (this.#cmp(data, p.data)) { p.left = newNd }
    else { p.right = newNd }
  }

  // O(log(n))
  get(data, eq = equal) {
    let nd = this.#root
    while (nd) {
      if (eq(nd.data, data)) { return nd.data }
      if (this.#cmp(data, nd.data)) { nd = nd.left }
      else { nd = nd.right }
    }
  }

  // O(log(n))
  remove(data, eq = equal) {
    const inOrderSucc = (nd) => {
      while (nd.left) { nd = nd.left }
      return nd
    }
    const removeNode = (nd, data) => {
      if (nd) {
        if (this.#cmp(data, nd.data)) {
          nd.left = removeNode(nd.left, data)
        } else if (this.#cmp(nd.data, data)) {
          nd.right = removeNode(nd.right, data)
        } else {
          // Leaf or a node with only one child
          if (!nd.left) { --this.length; return nd.right}
          if (!nd.right) { --this.length; return nd.left}
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

export class GNode {
  name = ""
  weight = 0
  adjs = []

  constructor(name, weight = 0) {
    this.name = name
    this.weight = weight
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

// const tr = BSTree.from([5, 9, 2, 8, 0, 4, 6, 3, 7, 1])
// for (const el of [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) {
//   console.log(tr.remove(el))
// }
// for (const el of tr.inOrder) { console.log(el) }
