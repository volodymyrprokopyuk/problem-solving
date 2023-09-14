import { inspect } from "util"
import { error, arrSwap } from "./util.js"

export class Heap {
  #arr = []
  #cmp

  constructor(cmp = (a, b) => a < b) { this.#cmp = cmp }

  get length() { return this.#arr.length }

  static from(it, cmp) {
    const heap = new Heap(cmp)
    for (const el of it) {
      Array.isArray(el) ? heap.push(el[0], el[1]) : heap.push(el, el)
    }
    return heap
  }

  [Symbol.iterator]() {
    function* elements(self) {
      while (self.#arr.length > 0) { yield self.pop() }
    }
    return elements(this)
  }

  [inspect.custom]() {
    return `Heap(${[...this].map(el => el.join(": ")).join(", ")})`
  }

  // O(log(n)) pushes an element to a heap
  push(key, data) {
    this.#arr.push([key, data])
    let i = this.#arr.length - 1
    while (i > 0) {
      const par = Math.floor((i - 1) / 2)
      if (this.#cmp(this.#arr[i][0], this.#arr[par][0])) { break }
      arrSwap(this.#arr, i, par)
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
            this.#cmp(this.#arr[ch1][0], this.#arr[ch2][0]) ? ch2 : ch1
      if (this.#cmp(this.#arr[i][0], this.#arr[par][0])) { break }
      arrSwap(this.#arr, i, par)
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
