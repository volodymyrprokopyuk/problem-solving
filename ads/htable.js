import { inspect } from "util"
import { djb2 } from "./util.js"
import { List } from "./list.js"

export class HTable {
  #arr
  #length = 0

  #hash(key) { return djb2(key) % this.#arr.length }

  #cmp([k], key) { return k === key }

  #rehash() {
    const old = [...this]
    this.#arr = Array(this.#arr.length * 2)
    this.#length = 0
    for (const [key, val] of old) { this.set(key, val) }
  }

  constructor(cap = 101) { this.#arr = Array(cap) }

  get length() { return this.#length }

  static from(it, cap) {
    const htb = cap ? new HTable(cap) : new HTable()
    for (const [key, val] of it) { htb.set(key, val) }
    return htb
  }

  [Symbol.iterator]() {
    const arr = []
    for (const lst of this.#arr) {
      if (lst) {
        let nd = lst.head
        while (nd) {
          arr.push(nd.data)
          nd = nd.next
        }
      }
    }
    let i = 0
    const next = () => {
      if (i < arr.length) {
        return { value: arr[i++], done: false }
      }
      return { done: true }
    }
    return { next }
  }

  [inspect.custom]() {
    return `HTable(${[...this].map(([k, v]) => `${k}: ${v}`)})`
  }

  // O(1) sets or updates a key to a value
  set(key, val) {
    if (this.#length > this.#arr.length * 0.75) { this.#rehash() }
    const kv = [key, val], i = this.#hash(key)
    let lst = this.#arr[i]
    if (lst) {
      const nd = lst.get(key, this.#cmp)
      if (nd) { nd.data = kv }
      else { lst.push(kv); ++this.#length }
    } else { this.#arr[i] = List.from([kv]); ++this.#length }
    return this
  }

  // O(1) returns a value for a key or undefined
  get(key) {
    const i = this.#hash(key), lst = this.#arr[i]
    if (lst) {
      const nd = lst.get(key, this.#cmp)
      if (nd) { return nd.data[1] }
    }
  }

  // O(1) deletes an element by a key or undefined
  delete(key) {
    const i = this.#hash(key), lst = this.#arr[i]
    if (lst) {
      const kv = lst.delete(key, this.#cmp)
      if (kv) { --this.#length; return kv }
    }
  }
}
