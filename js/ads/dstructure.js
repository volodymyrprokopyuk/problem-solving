function error(msg) { throw new Error(msg) }

export class Stack {
  constructor() { this.arr = [] }

  get length() { return this.arr.length }

  push(el) { return this.arr.push(el) }

  pop() { return this.arr.pop() }

  peek() { return this.arr.at(-1) }

  static from(it) {
    const st = new Stack()
    for (const el of it) { st.push(el) }
    return st
  }

  [Symbol.iterator]() {
    const next = () => {
      const done = this.length === 0
      return { value: this.pop(), done }
    }
    return { next }
  }
}

class LNode {
  constructor (data) {
    this.data = data
    this.next = null
    this.prev = null
  }
}

// preserves (does not reverse) the append order
export class LList {
  constructor() { this.tail = this.head = null }

  // O(1)
  append(vl) {
    const nd = new LNode(vl)
    if (this.head === null) { this.tail = this.head = nd; return }
    this.tail.next = nd
    this.tail = nd
  }

  // O(n)
  remove(vl) {
    let n = this.head
    if (n === null) { error("remove from empty linked list") }
    if (n.data === vl) { this.head = this.head.next; return }
    while (n.next !== null) {
      if (n.next.data === vl) {
        if (n.next === this.tail) { this.tail = n }
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

const ll = LList.from([1, 2, 3, 4, 5])
ll.remove(1)
ll.remove(3)
ll.remove(5)
ll.remove(99)
ll.append(10)
console.log(Array.from(ll))
