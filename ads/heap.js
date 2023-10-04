import { inspect } from "util"
import { error } from "./util.js"
import { swap } from "./array.js"

export class Heap {
  #arr = []
  #cmp

  constructor(cmp = (a, b) => a < b) { this.#cmp = cmp }

  get length() { return this.#arr.length }

  static from(it, cmp) {
    const heap = new Heap(cmp)
    for (const el of it) { heap.push(el) }
    return heap
  }

  [Symbol.iterator]() { return this.values() }

  values() {
    function* values(self) {
      while (self.#arr.length > 0) { yield self.pop() }
    }
    return values(this)
  }

  [inspect.custom]() { return `Heap(${[...this].join(", ")})` }

  // O(log(n)) pushes an element to a heap
  push(value) {
    this.#arr.push(value)
    let i = this.#arr.length - 1
    while (i > 0) {
      const par = Math.floor((i - 1) / 2)
      if (this.#cmp(this.#arr[par], this.#arr[i])) { break }
      swap(this.#arr, i, par)
      i = par
    }
    return this
  }

  // O(log(n)) pops a max/min element from a heap
  pop() {
    if (this.#arr.length === 0) { error("pop from empty heap") }
    const el = this.#arr[0]
    this.#arr[0] = this.#arr.at(-1)
    --this.#arr.length
    let par = 0, ch1 = par * 2 + 1, ch2 = ch1 + 1
    while (ch1 < this.#arr.length) {
      const i = this.#arr[ch2] &&
            this.#cmp(this.#arr[ch2], this.#arr[ch1]) ? ch2 : ch1
      if (this.#cmp(this.#arr[par], this.#arr[i])) { break }
      swap(this.#arr, i, par)
      par = i; ch1 = par * 2 + 1; ch2 = ch1 + 1
    }
    return el
  }

  // O(1) peeks a max/min element from a heap
  peek() {
    if (this.#arr.length === 0) { error("peek from empty heap") }
    return this.#arr[0]
  }
}
