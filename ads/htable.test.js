import { describe, test, expect } from "vitest"
import { HTable } from "./htable.js"

describe("HTable", () => {
  test("HTable from/length/iterator/set/get/delete", () => {
    let htb = HTable.from([["a", 1], ["b", 2], ["c", 3], ["a", 10]])
    expect(htb.length).toBe(3)
    expect([htb.get("a"), htb.get("b"), htb.get("c"), htb.get("x")])
      .toEqual([10, 2, 3, undefined])
    expect([htb.delete("a"), htb.delete("b"), htb.delete("c"), htb.delete("x")])
      .toEqual([["a", 10], ["b", 2], ["c", 3], undefined])
    htb = HTable.from([], 7)
    for (const el of Array(20).keys()) { htb.set(el, el) }
    expect(htb.get(14)).toBe(14)
  })
})
