package ads

import "fmt"

type BSTree struct {
  root *Node
}

func (t *BSTree) FromSlice(slc []int) {
  for _, val := range slc {
    t.Set(val)
  }
}

func (t *BSTree) InOrder() func(yield func(i, val int) bool) {
  i, more := 0, true
  return func(yield func(i, val int) bool) {
    var inOrder func(nd *Node)
    inOrder = func(nd *Node) {
      if nd != nil {
        inOrder(nd.left)
        if !more {
          return
        }
        more = yield(i, nd.value)
        i++
        inOrder(nd.right)
      }
    }
    inOrder(t.root)
  }
}

func (t *BSTree) PreOrder() func(yield func(i, val int) bool) {
  i, more := 0, true
  return func(yield func(i, val int) bool) {
    var preOrder func(nd *Node)
    preOrder = func(nd *Node) {
      if nd != nil {
        if !more {
          return
        }
        more = yield(i, nd.value)
        i++
        preOrder(nd.left)
        preOrder(nd.right)
      }
    }
    preOrder(t.root)
  }
}

func (t *BSTree) PostOrder() func(yield func(i, val int) bool) {
  i, more := 0, true
  return func(yield func(i, val int) bool) {
    var postOrder func(nd *Node)
    postOrder = func(nd *Node) {
      if nd != nil {
        postOrder(nd.left)
        postOrder(nd.right)
        if !more {
          return
        }
        more = yield(i, nd.value)
        i++
      }
    }
    postOrder(t.root)
  }
}

// func (t *BSTree) LevelOrder() func(yield func(i, val int) bool) {
//   i, more := 0, true
//   var que Queue
//   return func(yield func(i, val int) bool) {
//     // TODO
//   }
// }

// O(log(n))
func (t *BSTree) Set(val int) {
  var set func(nd *Node) *Node
  set = func(nd *Node) *Node {
    if nd == nil {
      return &Node{value: val}
    }
    switch {
    case val < nd.value:
      nd.left = set(nd.left)
    case val > nd.value:
      nd.right = set(nd.right)
    default:
      nd.value = val
    }
    return nd
  }
  t.root = set(t.root)
}

// O(log(n))
func (t *BSTree) Get(val int) (int, error) {
  var get func(nd *Node) (int, error)
  get = func(nd *Node) (int, error) {
    if nd == nil {
      return 0, fmt.Errorf("value %v not found in bstree", val)
    }
    if val < nd.value {
      return get(nd.left)
    }
    if val > nd.value {
      return get(nd.right)
    }
    return nd.value, nil
  }
  return get(t.root)
}

// O(log(n))
func (t *BSTree) Delete(val int) error {
  inOrderSucc := func(nd *Node) *Node {
    for nd.left != nil {
      nd = nd.left
    }
    return nd
  }
  var del func(nd *Node, val int) (*Node, error)
  del = func(nd *Node, val int) (*Node, error) {
    if nd == nil {
      return nil, fmt.Errorf("value %v not found in bstree", val)
    }
    var err error
    // binary search
    switch {
    case val < nd.value:
      nd.left, err = del(nd.left, val)
    case val > nd.value:
      nd.right, err = del(nd.right, val)
    // node found
    default:
      // leaf node
      if nd.left == nil && nd.right == nil {
        return nil, nil
      }
      // one-child node
      if nd.left == nil {
        return nd.right, nil
      }
      if nd.right == nil {
        return nd.left, nil
      }
      // two-children node
      succ := inOrderSucc(nd.right)
      nd.value = succ.value
      // remove the in-order successor
      nd.right, _ = del(nd.right, succ.value)
    }
    return nd, err
  }
  var err error
  t.root, err = del(t.root, val)
  return err
}

func (t *BSTree) Min() (int, error) {
  min := func(nd *Node) *Node {
    for nd.left != nil {
      nd = nd.left
    }
    return nd
  }
  if t.root == nil {
    return 0, fmt.Errorf("min from empty bstree")
  }
  nd := min(t.root)
  return nd.value, nil
}

func (t *BSTree) Max() (int, error) {
  max := func(nd *Node) *Node {
    for nd.right != nil {
      nd = nd.right
    }
    return nd
  }
  if t.root == nil {
    return 0, fmt.Errorf("min from empty bstree")
  }
  nd := max(t.root)
  return nd.value, nil
}

type TrieNode struct {
  runes map[rune]*TrieNode
}

func NewTrieNode() *TrieNode {
  return &TrieNode{runes: make(map[rune]*TrieNode, 52)}
}

type Trie struct {
  root *TrieNode
}

func NewTrie() *Trie {
  return &Trie{root: NewTrieNode()}
}

// O(key.length)
func (t *Trie) Set(key string) {
  nd := t.root
  for _, r := range key {
    if _, exist := nd.runes[r]; !exist {
      nd.runes[r] = NewTrieNode()
    }
    nd = nd.runes[r]
  }
}

// O(key.length)
func (t *Trie) Get(key string) bool {
  nd := t.root
  for _, r := range key {
    if _, exist := nd.runes[r]; !exist {
      return false
    }
    nd = nd.runes[r]
  }
  return true
}
