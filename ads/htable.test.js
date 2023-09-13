import { describe, test, expect } from "vitest"
import { HTable, HSet } from "./htable.js"

describe("HTable", () => {
  test("HTable equal/set/get/delete", () => {
    let htb = HTable.from([["a", 1], ["b", 2], ["c", 3], ["a", 10]])
    expect(htb.length).toBe(3)
    expect([...htb]).toEqual([["a", 10], ["b", 2], ["c", 3]])
    expect([htb.get("a"), htb.get("b"), htb.get("c"), htb.get("x")])
      .toEqual([10, 2, 3, undefined])
    expect([htb.delete("a"), htb.delete("b"), htb.delete("c"), htb.delete("x")])
      .toEqual([["a", 10], ["b", 2], ["c", 3], undefined])
    const arr = [["a", 1], ["b", 2], ["c", 3]]
    htb = HTable.from(arr)
    const htb2 = HTable.from(arr)
    expect(htb.equal(htb2)).toBe(true)
    htb2.set("a", 10)
    expect(htb.equal(htb2)).toBe(false)
    htb2.delete("a")
    expect(htb.equal(htb2)).toBe(false)
    htb = HTable.from(Array(20).keys(), 7)
    expect(htb.get(14)).toBe(14)
  })
})

describe("HSet", () => {
  test("HSet equal/set/get/delete", () => {
    const arr = [1, 2, 3, 4]
    const set = HSet.from(arr)
    expect(set.length).toBe(4)
    expect([...set]).toEqual([1, 2, 3, 4])
    const set2 = HSet.from(arr)
    expect(set.equal(set2)).toBe(true)
    set2.delete(2)
    expect(set.equal(set2)).toBe(false)
    set2.set(5)
    expect(set.equal(set2)).toBe(false)
    expect([set.get(1), set.get(4), set.get(-1)])
      .toEqual([true, true, undefined])
    expect([set.delete(1), set.delete(4), set.delete(-1)])
      .toEqual([true, true, undefined])
  })

  test("HSet union/isect/diff/sdiff/subset", () => {
    const a = HSet.from([1, 2, 3, 4])
    const b = HSet.from([3, 4, 5, 6])
    expect([...a.union(b)]).toEqual([1, 2, 3, 4, 5, 6])
    expect([...a.isect(b)]).toEqual([3, 4])
    expect([...a.diff(b)]).toEqual([1, 2])
    expect([...b.diff(a)]).toEqual([5, 6])
    expect([...a.sdiff(b)]).toEqual([1, 2, 5, 6])
    expect(HSet.from([1, 2]).subset(a)).toBe(true)
    expect(HSet.from([1, 9]).subset(a)).toBe(false)
  })
})
