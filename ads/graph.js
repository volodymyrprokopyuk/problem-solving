import { Stack } from "./stack.js"
import { Queue } from "./queue.js"
import { HSet } from "./htable.js"

export class GNode {
  key; value
  nodes = []

  constructor (key, value) { this.key = key; this.vallue = value }
}

// O(v+e) explores once deeper vertices first before going wider
export function* depthFirstSearch(nd, key = false) {
  const stk = Stack.from([nd]), visited = HSet.from([nd.key])
  while (stk.length > 0) {
    const nd = stk.pop()
    yield key ? nd.key : nd
    for (const n of nd.nodes) {
      if (!visited.get(n.key)) { stk.push(n); visited.set(n.key) }
    }
  }
}

// O(v+e) explores once adjacent vertices first before going deeper
export function* breadthFirstSearch(nd, key = false) {
  const que = Queue.from([nd]), visited = HSet.from([nd.key])
  while (que.length > 0) {
    const nd = que.deq()
    yield key ? nd.key : nd
    for (const n of nd.nodes) {
      if (!visited.get(n.key)) { que.enq(n); visited.set(n.key) }
    }
  }
}

// O(v+e) enumerates all graph nodes once
export function* graphNodes(nodes, key = false, search = breadthFirstSearch) {
  const visited = new HSet()
  for (const nd of nodes) {
    for (const n of search(nd)) {
      if (!visited.get(n.key)) {
        visited.set(n.key)
        yield key ? n.key : n
      }
    }
  }
}

// O(v+e) returns a topological sort of a DAG represented by in-degree 0 nodes
export function topologicalSort(nodes) {
  for (const nd of graphNodes(nodes)) { nd.indeg = 0 }
  for (const nd of graphNodes(nodes)) {
    for (const n of nd.nodes) { ++n.indeg }
  }
  const que = new Queue()
  for (const nd of graphNodes(nodes)) {
    if (nd.indeg === 0) { que.enq(nd) }
  }
  const topSort = []
  while (que.length > 0) {
    const nd = que.deq()
    topSort.push(nd)
    for (const n of nd.nodes) {
      --n.indeg
      if (n.indeg === 0) { que.enq(n) }
    }
  }
  return topSort
}

// const a = new GNode("a"), b = new GNode("b"), c = new GNode("c"),
//       d = new GNode("d"), e = new GNode("e"), f = new GNode("f"),
//       g = new GNode("g")
// a.nodes.push(b)
// b.nodes.push(c, d, e)
// c.nodes.push(e)
// d.nodes.push(e)
// e.nodes.push(f)
// g.nodes.push(d)
// console.log(topologicalSort([a, g]).map(({ key }) => key))

// const a = new GNode("a"), b = new GNode("b"), c = new GNode("c"),
//       d = new GNode("d"), e = new GNode("e"), f = new GNode("f"),
//       g = new GNode("g")
// a.nodes.push(b, c, d)
// b.nodes.push(e)
// c.nodes.push(b)
// d.nodes.push(c, f)
// e.nodes.push(c)
// f.nodes.push(g)
// g.nodes.push(d)
// console.log(...breadthFirstSearch(a, true))
// console.log(...breadthFirstSearch(g, true))
// console.log(...depthFirstSearch(a, true))
// console.log(...depthFirstSearch(g, true))
// console.log(...graphNodes([a], true))
