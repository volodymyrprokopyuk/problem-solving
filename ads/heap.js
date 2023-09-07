import { inspect } from "util"
import { error, arrSwap } from "./util.js"

export class Heap {
  #arr = []
  #cmp

  constructor(cmp = (a, b) => a < b) { this.#cmp = cmp }

  get length() { return this.#arr.length }

  static from(it, cmp) {
    const heap = cmp ? new Heap(cmp) : new Heap()
    for (const el of it) { heap.push(el) }
    return heap
  }

  [Symbol.iterator]() {
    const next = () => {
      if (this.#arr.length > 0) {
        return { value: this.pop(), done: false }
      }
      return { done: true }
    }
    return { next }
  }

  [inspect.custom]() { return `Heap(${[...this]})` }

  // O(log(n)) pushes an element to a heap
  push(data) {
    this.#arr.push(data)
    let i = this.#arr.length - 1
    while (i > 0) {
      const par = Math.floor((i - 1) / 2)
      if (this.#cmp(this.#arr[i], this.#arr[par])) { break }
      arrSwap(this.#arr, i, par)
      i = par
    }
    return this
  }

  // O(log(n)) pops a max/min element from a heap
  pop() {
    if (this.#arr.length === 0) { error("pop from empty heap") }
    const data = this.#arr[0]
    this.#arr[0] = this.#arr.at(-1)
    --this.#arr.length
    let par = 0, ch1 = par * 2 + 1, ch2 = ch1 + 1
    while (ch1 < this.#arr.length) {
      const i = this.#arr[ch2] && this.#cmp(this.#arr[ch1], this.#arr[ch2]) ?
            ch2 : ch1
      if (this.#cmp(this.#arr[i], this.#arr[par])) { break }
      arrSwap(this.#arr, i, par)
      par = i; ch1 = par * 2 + 1; ch2 = ch1 + 1
    }
    return data
  }

  // O(1) peeks a max/min element from a heap
  peek() {
    if (this.#arr.length === 0) { error("peek from empty heap") }
    return this.#arr[0]
  }
}
