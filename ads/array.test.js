import { describe, test, expect } from "vitest"
import {
  matrix, merge, hoarePartition, lomutoPartition, quickSelect,
  permutations, permutations2, powerset, powerset2, binarySearch,
  bubbleSort, insertionSort, insertionSort2, shellSort, selectionSort,
  mergeSort, quickSort, quickSort2, heapSort, bstSort, radixSortNum
} from "./array.js"

describe("matrix", () => {
  test("matrix", () => {
    const mat = matrix(2, 3, 9)
    expect(mat[1][2]).toBe(9)
  })
})

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
    [[2, 1, 3, 2], 1, [1, 2, 3, 2]],
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

describe("permutations", () => {
  const arr = ["a", "b", "c"], exp = [
    [ "a", "b", "c" ],
    [ "a", "c", "b" ],
    [ "b", "a", "c" ],
    [ "b", "c", "a" ],
    [ "c", "a", "b" ],
    [ "c", "b", "a" ]
  ]
  test("permutations", () => { expect(permutations(arr)).toEqual(exp) })
  test("permutations2", () => { expect(permutations2(arr)).toEqual(exp) })
})

describe("powerset", () => {
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

describe("binarySearch", () => {
  test("binarySearch", () => {
    const arr = [...Array(10).keys()]
    expect([0, 1, 2, 5, 6, 7, 9, 10].map(val => binarySearch(val, arr)))
      .toEqual([0, 1, 2,  5, 6, 7, 9, -1])
  })
})

describe("bubbleSort", () => {
  test("bubbleSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    bubbleSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("insertionSort", () => {
  test("insertionSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    insertionSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("insertionSort2", () => {
  test("insertionSort2", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    insertionSort2(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("shellSort", () => {
  test("shellSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    shellSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("selectionSort", () => {
  test("selectionSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    selectionSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("mergeSort", () => {
  test("mergeSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    expect(mergeSort(arr)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("quickSort", () => {
  test("quickSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    quickSort(arr)
    expect(arr).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
  test("quickSort2", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    expect(quickSort2(arr)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("heapSort", () => {
  test("heapSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    expect(heapSort(arr)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("bstSort", () => {
  test("bstSort", () => {
    const arr = [9, 4, 5, 2, 8, 3, 6, 1, 7, 0]
    expect(bstSort(arr)).toEqual([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
  })
})

describe("radixSortNum", () => {
  test.each(
    [[[9, 4, 5, 2, 8, 3, 6, 1, 7, 0, 10],
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]],
     [[4, 100, 54, 537, 2, 89], [2, 4, 54, 89, 100, 537]],
     [[345, 654, 924, 123, 567, 472, 555, 808, 911, 1000],
      [123, 345, 472, 555, 567, 654, 808, 911, 924, 1000]]]
  )("radixSortNum(%j) === %j", (arr, exp) => {
    expect(radixSortNum(arr)).toEqual(exp)
  })
})
