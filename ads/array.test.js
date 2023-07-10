import { describe, test, expect } from "vitest"
import {
  merge,
  hoarePartition, lomutoPartition, quickSelect,
  powerset, powerset2
} from "./array.js"

describe("array merge", () => {
  test.each([
    [[], [], []],
    [[1, 3], [2], [1, 2, 3]],
    [[1, 4, 7], [2, 3, 5, 8, 9], [1, 2, 3, 4, 5, 7, 8, 9]]
  ])("%# merge(%j, %j) === %j", (a, b, exp) => {
    expect(merge(a, b)).toEqual(exp)
  })
})

describe("Hoare partition", () => {
  test.each([
    [[], undefined, []],
    [[1], 0, [1]],
    [[1, 2], 0, [1, 2]],
    [[2, 1], 1, [1, 2]],
    [[2, 3, 1], 1, [1, 2, 3]],
    [[2, 1, 3, 2], 2, [2, 1, 2, 3]],
    [[5, 9, 1, 3, 7, 8, 4, 6, 2], 4, [4, 2, 1, 3, 5, 8, 7, 6, 9]]
  ])("%# hoarePartition(%j) === [%j, %j]", (arr, expP, expArr) => {
    expect(hoarePartition(arr)).toBe(expP)
    expect(arr).toEqual(expArr)
  })
})

describe("Lomuto partition", () => {
  test.each([
    [[], undefined, []],
    [[1], 0, [1]],
    [[1, 2], 1, [1, 2]],
    [[2, 1], 0, [1, 2]],
    [[2, 3, 1], 0, [1, 3, 2]],
    [[2, 1, 3, 2], 2, [2, 1, 2, 3]],
    [[5, 9, 1, 3, 7, 8, 4, 6, 2], 1, [1, 2, 5, 3, 7, 8, 4, 6, 9]]
  ])("%# lomutoPartition(%j) === [%j, %j]", (arr, expP, expArr) => {
    expect(lomutoPartition(arr)).toBe(expP)
    expect(arr).toEqual(expArr)
  })
})

describe("quick select", () => {
  test.each([
    [[], 0, [], undefined],
    [[1], 0, [1], 0],
    [[1, 2], 1, [1, 2], 1],
    [[2, 1], 1, [1, 2], 1],
    [[2, 3, 1], 2, [1, 2, 3], 2],
    [[2, 1, 3, 2], 2, [2, 1, 2, 3], 2],
    [[5, 9, 1, 3, 7, 8, 4, 6, 2], 5, [4, 2, 1, 3, 5, 6, 7, 8, 9], 5]
  ])("%# quickSelect(%j, %j) === [%j, %j]", (arr, k, expArr, expP) => {
    expect(quickSelect(arr, k)).toBe(expP)
    expect(arr).toEqual(expArr)
  })
})

describe("powerset", (arr, exp) => {
  test.each([
    ["abc", ["", "a", "b", "ab", "c", "ac", "bc", "abc"]]
  ])("%# powerset(%j) === %j", (arr, exp) => {
    expect(powerset(arr.split("")).map(el => el.join(""))).toEqual(exp)
  })
  test.each([
    ["abc", ["", "c", "b", "bc", "a", "ac", "ab", "abc"]]
  ])("%# powerset2(%j) === %j", (arr, exp) => {
    expect(powerset2(arr.split("")).map(el => el.join(""))).toEqual(exp)
  })
})
