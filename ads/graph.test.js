import { describe, test, expect } from "vitest"
import { GNode, depthFirstSearch, breadthFirstSearch } from "./graph.js"

describe("graph traversals", () => {
  const a = new GNode("a"), b = new GNode("b"), c = new GNode("c"),
        d = new GNode("d"), e = new GNode("e"), f = new GNode("f"),
        g = new GNode("g")
  a.nodes.push(b, c, d)
  b.nodes.push(e)
  c.nodes.push(b)
  d.nodes.push(c, f)
  e.nodes.push(c)
  f.nodes.push(g)
  g.nodes.push(d)
  test("DFS", () => {
    expect([...depthFirstSearch(a, true)])
      .toEqual(["a", "d", "f", "g", "c", "b", "e"])
    expect([...depthFirstSearch(g, true)])
      .toEqual(["g", "d", "f", "c", "b", "e"])
  })
  test("BFS", () => {
    expect([...breadthFirstSearch(a, true)])
      .toEqual(["a", "b", "c", "d", "e", "f", "g"])
    expect([...breadthFirstSearch(g, true)])
      .toEqual(["g", "d", "c", "f", "b", "e"])
  })
})
