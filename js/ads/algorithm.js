import { Stack } from "./dstructure.js"

export function reverse(arr) {
  let res = []
  for (let i = arr.length - 1; i >= 0; --i) { res.push(arr[i]) }
  return res
}

export function reverse2(arr) { return Array.from(Stack.from(arr)) }

export function checkParens(
  str, parens = { "(": ")", "[": "]", "{": "}", "<": ">" }
) {
  const cl = new Set(Object.values(parens))
  const st = new Stack()
  for (const ch of str) {
    if (ch in parens) {
      st.push(ch)
    } else if (cl.has(ch)) {
      if (st.length === 0) { throw new Error(`extra close ${ch}`) }
      const op = st.pop()
      if (ch !== parens[op]) { throw new Error(`mismatch ${op} ${ch}`) }
    }
  }
  if (st.length !== 0) { throw new Error(`extra open ${st.peek()}`) }
  return true
}

// const strs = ["", "abc", "a(b{c[d<e>f]g}h)i", "a(", "a(b]", "abc)"]
// for (const str of strs) {
//   try { console.log(checkParens(str)) }
//   catch (err) { console.error(err.message) }
// }
