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
    const node = () => {
      const value = nd.data
      nd = nd.next
      return { value, done: false }
    }
    let i = -1, nd = null
    const next = () => {
      if (nd) { return node() }
      while (++i < this.#arr.length &&
             (!this.#arr[i] || this.#arr[i].length === 0));
      if (i < this.#arr.length) {
        nd = this.#arr[i].head
        if (nd) { return node() }
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

export class HSet extends HTable {
  static from (it, cap) {
    const set = cap ? new HSet(cap) : new HSet()
    for (const el of it) { set.set(el) }
    return set
  }

  [Symbol.iterator]() {
    const it = super[Symbol.iterator]()
    const next = () => {
      const { value, done } = it.next()
      return done ? { done } : { value: value[0], done }
    }
    return { next }
  }

  [inspect.custom]() { return `HSet(${[...this]})` }

  // O(1) adds an element to a set
  set(key) { return super.set(key, true) }

  // O(1) deletes an element or undefined
  delete(key) {
    const del = super.delete(key)
    return del ? del[1] : del
  }

  // O(m + n) returns a union of this set and an iterable
  union(it) {
    const union = HSet.from(this)
    for (const el of it) { union.set(el) }
    return union
  }

  // O(n) returns an intersection of this set and an iterable
  isect(it) {
    const isect = new HSet()
    for (const el of it) {
      if (this.get(el)) { isect.set(el) }
    }
    return isect
  }

  // O(m + n) returns a difference of this set and an iterable
  diff(it) {
    const diff = HSet.from(this)
    for (const el of it) {
      if (diff.get(el)) { diff.delete(el) }
    }
    return diff
  }

  // O(m + n) returns a symmetric difference of this set and an iterable
  sdiff(it) {
    const sdiff = HSet.from(this)
    for (const el of it) {
      sdiff.get(el) ? sdiff.delete(el) : sdiff.set(el)
    }
    return sdiff
  }
}
