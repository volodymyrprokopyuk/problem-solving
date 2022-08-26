export class Stack {
  constructor() { this.arr = [] }
  get length() { return this.arr.length }
  push(e) { return this.arr.push(e) }
  pop(e) { return this.arr.pop(e) }
  peek() { return this.arr.at(-1) }
}

const st = new Stack()
st.push(1)
st.push(2)
console.log(st, st.length)
console.log(st.pop(), st.peek(), st.pop(), st.length)
console.log(st.peek(), st.pop())
