import { error } from "./util.js"
import { Stack } from "./stack.js"

export function checkParens(str, parens = { "(": ")", "[": "]", "{": "}" }) {
  const op = Object.keys(parens), cl = Object.values(parens)
  const stk = new Stack()
  for (const ch of str) {
    if (op.includes(ch)) { stk.push(ch); continue }
    if (cl.includes(ch)) {
      if (stk.length === 0) { error(`extra close ${ch}`) }
      if (ch === parens[stk.peek()]) { stk.pop() }
      else { error(`invalid close ${ch} expected ${parens[stk.peek()]}`) }
      continue
    }
  }
  if (stk.length !== 0) { error(`extra paren ${stk.peek()}`) }
  return true
}
