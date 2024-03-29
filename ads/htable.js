import { inspect } from "util"
import { List } from "./list.js"

// O(n) non-cryptographic hash function
export function djb2(str) {
  let hash = 5381
  for (const ch of String(str)) {
    hash = hash * 33 + ch.charCodeAt(0)
  }
  return hash
}

export class HTable {
  #arr
  #length = 0

  #hash(key) { return djb2(key) % this.#arr.length }

  #cmp(key, [k, _]) { return key === k }

  #rehash() {
    const old = [...this]
    this.#arr = Array(this.#arr.length * 2)
    this.#length = 0
    for (const [key, val] of old) { this.set(key, val) }
  }

  constructor(cap = 101) { this.#arr = Array(cap) }

  get length() { return this.#length }

  static from(it, cap) {
    const htb = new HTable(cap)
    for (const el of it) {
      Array.isArray(el) ? htb.set(el[0], el[1]) : htb.set(el, el)
    }
    return htb
  }

  [Symbol.iterator]() { return this.entries() }

  entries() {
    function* entries(arr) {
      for (const lst of arr) {
        if (lst) {
          for (const kv of lst) { yield kv }
        }
      }
    }
    return entries(this.#arr)
  }

  keys() {
    function* keys(arr) {
      for (const lst of arr) {
        if (lst) {
          for (const [key, _] of lst) { yield key }
        }
      }
    }
    return keys(this.#arr)
  }

  values() {
    function* values(arr) {
      for (const lst of arr) {
        if (lst) {
          for (const [_, value] of lst) { yield value }
        }
      }
    }
    return values(this.#arr)
  }

  [inspect.custom]() {
    return `HTable(${[...this].map(kv => kv.join(": ")).join(", ")})`
  }

  // O(1) sets or updates a key to a value
  set(key, val) {
    if (this.#length > this.#arr.length * 0.75) { this.#rehash() }
    const kv = [key, val], i = this.#hash(key)
    let lst = this.#arr[i]
    if (lst) {
      const nd = lst.get(key, this.#cmp)
      if (nd) { nd.value = kv }
      else { lst.push(kv); ++this.#length }
    } else { this.#arr[i] = List.from([kv]); ++this.#length }
    return this
  }

  // O(1) returns a value for a key or undefined
  get(key) {
    const i = this.#hash(key), lst = this.#arr[i]
    if (lst) {
      const nd = lst.get(key, this.#cmp)
      if (nd) { return nd.value[1] }
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

  // O(n) returns true if hash tables are equal
  equal(htb) {
    if (this.#length !== htb.length) { return false }
    for (const [key, val] of htb) {
      if (val !== this.get(key)) { return false }
    }
    return true
  }
}

export class HSet extends HTable {
  static from (it, cap) {
    const set = new HSet(cap)
    for (const el of it) { set.set(el) }
    return set
  }

  [Symbol.iterator]() { return super.keys() }

  [inspect.custom]() { return `HSet(${[...this].join(", ")})` }

  // O(1) adds an element to a set
  set(key) { return super.set(key, true) }

  // O(1) deletes an element or undefined
  delete(key) {
    const del = super.delete(key)
    return del ? del[1] : del
  }

  subset(set) {
    for (const el of this) {
      if (!set.get(el)) { return false }
    }
    return true
  }

  equal(set) {
    if (this.length !== set.length) { return false }
    return this.subset(set)
  }

  // O(n+m) returns a union of this set and an iterable
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

  // O(n+m) returns a difference of this set and an iterable
  diff(it) {
    const diff = HSet.from(this)
    for (const el of it) {
      if (diff.get(el)) { diff.delete(el) }
    }
    return diff
  }

  // O(n+m) returns a symmetric difference of this set and an iterable
  sdiff(it) {
    const sdiff = HSet.from(this)
    for (const el of it) {
      sdiff.get(el) ? sdiff.delete(el) : sdiff.set(el)
    }
    return sdiff
  }
}
