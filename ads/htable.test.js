import { describe, test, expect } from "vitest"
import { HTable, HSet } from "./htable.js"

describe("HTable", () => {
  test("HTable from/length/iterator/equal/set/get/delete", () => {
    let htb = HTable.from([["a", 1], ["b", 2], ["c", 3], ["a", 10]])
    expect(htb.length).toBe(3)
    const htb2 = HTable.from([["a", 10], ["b", 2], ["c", 3]])
    expect(htb.equal(htb2)).toBe(true)
    htb2.set("a", 9)
    expect(htb.equal(htb2)).toBe(false)
    htb2.delete("a")
    expect(htb.equal(htb2)).toBe(false)
    expect([htb.get("a"), htb.get("b"), htb.get("c"), htb.get("x")])
      .toEqual([10, 2, 3, undefined])
    expect([htb.delete("a"), htb.delete("b"), htb.delete("c"), htb.delete("x")])
      .toEqual([["a", 10], ["b", 2], ["c", 3], undefined])
    htb = HTable.from([], 7)
    for (const el of Array(20).keys()) { htb.set(el, el) }
    expect(htb.get(14)).toBe(14)
  })
})

describe("HSet", () => {
  test("HSet from/length/iterator/set/get/delete", () => {
    const set = HSet.from([1, 2, 3, 4])
    expect(set.length).toBe(4)
    expect([...set]).toEqual([1, 2, 3, 4])
    expect([set.get(1), set.get(4), set.get(9)])
      .toEqual([true, true, undefined])
    expect([set.delete(1), set.delete(4), set.delete(9)])
      .toEqual([true, true, undefined])
  })

  test("HSet union/isect/diff/sdiff", () => {
    const a = HSet.from([1, 2, 3, 4])
    const b = HSet.from([3, 4, 5, 6])
    expect([...a.union(b)]).toEqual([1, 2, 3, 4, 5, 6])
    expect([...a.isect(b)]).toEqual([3, 4])
    expect([...a.diff(b)]).toEqual([1, 2])
    expect([...b.diff(a)]).toEqual([5, 6])
    expect([...a.sdiff(b)]).toEqual([1, 2, 5, 6])
  })
})
