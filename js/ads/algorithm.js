import { Stack } from "./dstructure.js"

function error(msg) { throw new Error(msg) }

export function reverse(arr) {
  let res = []
  for (let i = arr.length - 1; i >= 0; --i) { res.push(arr[i]) }
  return res
}

export function reverse2(arr) { return Array.from(Stack.from(arr)) }

// const arr = [1, 2, 3, 4, 5]
// console.log(reverse([]), reverse(arr), reverse2([]), reverse2(arr))

export function checkParens(
  str, parens = { "(": ")", "[": "]", "{": "}", "<": ">" }
) {
  const cl = new Set(Object.values(parens))
  const st = new Stack()
  for (const ch of str) {
    if (ch in parens) { st.push(ch) }
    else if (cl.has(ch)) {
      if (st.length === 0) { error(`extra close ${ch}`) }
      const op = st.pop()
      if (ch !== parens[op]) { error(`mismatch ${op} ${ch}`) }
    }
  }
  if (st.length !== 0) { error(`extra open ${st.peek()}`) }
  return true
}

// const strs = ["", "abc", "a(b{c[d<e>f]g}h)i", "a(", "a(b]", "abc)"]
// for (const str of strs) {
//   try { console.log(checkParens(str)) }
//   catch (err) { console.error(err.message) }
// }
