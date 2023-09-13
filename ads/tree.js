export class TNode {
  key; data
  left = null; right = null

  constructor(key, data) { this.key = key; this.data = data }
}

export class BSTree {
  #root = null
  #length = 0
  #cmp

  constructor (cmp = (a, b) => a < b) { this.#cmp = cmp }

  get length() { return this.#length }

  static from(it, cmp) {
    const bst = new BSTree(cmp)
    for (const el of it) {
      Array.isArray(el) ? bst.set(el[0], el[1]) : bst.set(el)
    }
    return bst
  }

  [Symbol.iterator]() {
    function* inOrder(nd) {
      if (nd) {
        yield* inOrder(nd.left)
        yield nd
        yield* inOrder(nd.right)
      }
    }
    return inOrder(this.#root)
  }

  // O(log(n)) inserts an element into a tree
  set(key, data) {
    const set = nd => {
      if (key === nd.key) { nd.data = data }
      else if (this.#cmp(key, nd.key)) {
        if (nd.left) { set(nd.left) }
        else { nd.left = new TNode(key, data); ++this.#length }
      } else {
        if (nd.right) { set(nd.right) }
        else { nd.right = new TNode(key, data); ++this.#length }
      }
    }
    if (this.#root) { set(this.#root) }
    else { this.#root = new TNode(key, data); ++this.#length }
    return this
  }

  // O(log(n)) returns a matching node
  get(key) {
    const get = nd => {
      if (nd) {
        return key === nd.key ? nd :
          this.#cmp(key, nd.key) ? get(nd.left) : get(nd.right)
      }
    }
    return get(this.#root)
  }
}

// const bst = BSTree.from([7, 3, 11, 5, 1, 10, 14, 1])
const bst = BSTree.from([[7, "s"], [3, "t"], [11, "e"], [5, "f"], [11, "E"]])
// [7, 3, 11, 5, -1].forEach(el => console.log(bst.get(el)?.key))
console.log([...bst].map(el => [el.key, el.data]))
