# Algorithms and data structures

## Data structures

### List

- `LNode { value; prev = null; next = null }`
- `List { #head = null; #length = 0 }`
- `from([value]); entries(): [LNode]; values(): [value]`
- `push(value): List` O(1)
- `pop(): value | error` O(1)
- `peek(): value | error` O(1)
- `delete(value, eq): value | undefined` O(n)
- `get(value, eq): LNode | undefined` O(n)
- `reverse()` O(n)

### DList

- `DList { #head = null; #tail = null; #length = 0 }`
- `from([value]); entries(reverse): [LNode]; values(reverse): [value]`
- `push(value): List` O(1)
- `pushTail(value): List` O(1)
- `pop(): value | error` O(1)
- `popTail(): value | error` O(1)
- `peek(): value | error` O(1)
- `peekTail(): value | error` O(1)
- `insert(node, value)` O(1)
- `delete(node)` O(1)
- `get(value, eq): LNode | undefined` O(n)

### Stack

- `Stack { #top = null; #length = 0 }`
- `from([value]); values(): [value]`
- `push(value): Stack` O(1)
- `pop(): value | error` O(1)
- `peek(): value | error` O(1)

### Queue

- `Queue { #front = null; #rear = null; #length = 0 }`
- `from([value]); values(): [value]`
- `enq(value): Queue` O(1)
- `deq(): value | error` O(1)
- `peek(): value | error` O(1)

### Deque

- `Deque { #front = null; #rear = null; #length = 0 }`
- `from([value]); values(): [value]`
- `enq(value): Queue` O(1)
- `enqFront(value): Queue` O(1)
- `deq(): value | error` O(1)
- `deqRear(): value | error` O(1)
- `peek(): value | error` O(1)
- `peekRear(): value | error` O(1)

### HTable

- `HTable { #arr = Array(cap); #length = 0 }`
- `from([[key, value]] | [key], cap)`
- `entries(): [[key, value]]; keys(): [key]; values(): [value]`
- `set(key, value): HTable` O(1)
- `get(key): value | undefined` O(1)
- `delete(key): [key, value] | undefined` O(1)
- `equal(htb): true | false` O(n)

### HSet

- `HSet { HTable }`
- `from([key], cap); keys(): [key]`
- `set(key): HSet` O(1)
- `get(key): true | undefined` O(1)
- `delete(key): key | undefined` O(1)
- `subset(set): true | false` O(n)
- `equal(set): true | false` O(n)
- `union(it): HSet` O(n+m)
- `isect(it): HSet` O(n)
- `diff(it): HSet` O(n+m)
- `sdiff(it): HSet` O(n+m)

### Heap

- `Heap { #arr = []; #cmp = (a, b) => a < b }`
- `from([value], cmp); values(): [value]`
- `push(value): Heap` O(log(n))
- `pop(): value | error` O(log(n))
- `peek(): value | error` O(1)

### BSTree

- `TNode { key; value; left = null; right = null }`
- `BSTree { #root = null; length = 0; #cmp = (a, b) => a < b }`
- `inOrder(node, key); preOrder(node, key); postOrder(node, key)`
- `from([[key, value]] | [key], cmp)`
- `set(key, value): BSTree` O(log(n))
- `get(key): TNode | undefined` O(log(n))
- `delete(key): [key, value] | undefined` O(log(n))
- `min(key): TNode | error` O(log(n))
- `max(key): TNode | error` O(log(n))

### Trie

- `Trie { #root = new HTable() }`
- `from([key]); keys(): [key]; words(prefix): [key] | undefined`
- `set(key): Trie` O(key.length)
- `get(key, word): true | undefined` O(key.length)
- `delete(key): key | undefined` O(key.length)

## Testing

```zsh
npm exec vitest run --reporter verbose --coverage file.test.js -t 'Test'
```
