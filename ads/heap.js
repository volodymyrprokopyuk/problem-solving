import { inspect } from "util"
import { error } from "./util.js"
import { swap } from "./array.js"
import { HTable } from "./htable.js"

export class Heap {
  #arr = []
  #cmp; #key; #htb

  constructor(cmp = (a, b) => a < b, key = undefined) {
    this.#cmp = cmp
    if (key) { this.#key = key; this.#htb = new HTable() }
  }

  get length() { return this.#arr.length }

  static from(it, cmp, key) {
    const heap = new Heap(cmp, key)
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

  #heapUp(i = this.#arr.length - 1) {
    const arr = this.#arr, cmp = this.#cmp
    while (i > 0) {
      const par = Math.floor((i - 1) / 2)
      if (cmp(arr[par], arr[i])) { break }
      swap(arr, i, par)
      if (this.#key) { this.#index(i); this.#index(par) }
      i = par
    }
  }

  #heapDown(par = 0) {
    const arr = this.#arr, cmp = this.#cmp
    let ch1 = par * 2 + 1, ch2 = ch1 + 1
    while (ch1 < arr.length) {
      const i = arr[ch2] && cmp(arr[ch2], arr[ch1]) ? ch2 : ch1
      if (cmp(arr[par], arr[i])) { break }
      swap(arr, i, par)
      if (this.#key) { this.#index(i); this.#index(par) }
      par = i; ch1 = par * 2 + 1; ch2 = ch1 + 1
    }
  }

  #index(i) { this.#htb.set(this.#key(this.#arr[i]), i) }

  // O(log(n)) pushes an element to a heap
  push(value) {
    this.#arr.push(value)
    if (this.#key) { this.#index(this.#arr.length - 1) }
    this.#heapUp()
    return this
  }

  // O(log(n)) pops a max/min element from a heap
  pop() {
    const arr = this.#arr
    if (arr.length === 0) { error("pop from empty heap") }
    const el = arr[0]
    if (this.#key) { this.#htb.delete(this.#key(arr[0])) }
    arr[0] = arr.at(-1)
    --arr.length
    this.#heapDown()
    return el
  }

  // O(1) peeks a max/min element from a heap
  peek() {
    if (this.#arr.length === 0) { error("peek from empty heap") }
    return this.#arr[0]
  }

  // O(log(n))
  decKey(key, value) {
    const i = this.#htb.get(key)
    if (!i) { error(`key ${key} is not in heap`) }
    this.#arr[i] = value
    this.#heapUp(i)
  }
}
