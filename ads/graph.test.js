import { describe, test, expect } from "vitest"
import {
  GNode, depthFirstSearch, breadthFirstSearch, graphNodes, topologicalSort
} from "./graph.js"

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
  test("graphNodes", () => {
    expect([...graphNodes([a], true)])
      .toEqual(["a", "b", "c", "d", "e", "f", "g"])
  })
})

describe("topological sort", () => {
  const a = new GNode("a"), b = new GNode("b"), c = new GNode("c"),
        d = new GNode("d"), e = new GNode("e"), f = new GNode("f"),
        g = new GNode("g")
  a.nodes.push(b)
  b.nodes.push(c, d, e)
  c.nodes.push(e)
  d.nodes.push(e)
  e.nodes.push(f)
  g.nodes.push(d)
  test("topologicalSort", () => {
    expect([...topologicalSort([a, g])].map(({ key }) => key))
      .toEqual(["a", "g", "b", "c", "d", "e", "f"])
  })
})
