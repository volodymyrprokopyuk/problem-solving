export class Stack {
  constructor() { this.arr = [] }

  get length() { return this.arr.length }

  push(e) { return this.arr.push(e) }

  pop(e) { return this.arr.pop(e) }

  peek() { return this.arr.at(-1) }

  static from(iter) {
    const st = new Stack()
    for (const el of iter) { st.push(el) }
    return st
  }

  [Symbol.iterator]() {
    return {
      next: () => {
        const done = this.length === 0
        return { value: this.pop(), done }
      }
    }
  }
}
