import { error } from "./util.js"
import { Stack } from "./stack.js"

// O(n) checks whether parens are balanced
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

// O(n) tokenizes a prefix, infix, postfix arithmethic expression
function tokenize(infix) {
  const tokens = []
  for (const token of infix.match(/\w|[-+*/^()]/g)) {
    const tk =
          /[-+]/.test(token) ? { token, type: "op", prcd: 1 } :
          /[*/]/.test(token) ? { token, type: "op", prcd: 2 } :
          /[\^]/.test(token) ? { token, type: "op", prcd: 3 } :
          /[()]/.test(token) ? { token, type: "par" } :
          { token, type: "val" }
    tokens.push(tk)
  }
  return tokens
}

// O(1) evaluates an arithmetic operation on a stack
function evaluate(op, stk) {
  switch (op) {
    case "+": op = (a, b) => a + b; break
    case "-": op = (a, b) => a - b; break
    case "*": op = (a, b) => a * b; break
    case "/": op = (a, b) => a / b; break
    case "^": op = (a, b) => a ** b; break
    default: error(`unsupported operation ${op}`)
  }
  const b = Number(stk.pop()), a = Number(stk.pop())
  stk.push(op(a, b))
}

// O(n) converts an infixt expression to a postfix expression
export function infixToPostfix(infix) {
  const postfix = [], op = new Stack()
  for (const tk of tokenize(infix)) {
    if (tk.type === "op") {
      while (op.length > 0  && op.peek().prcd >= tk.prcd) {
        postfix.push(op.pop().token)
      }
      op.push(tk)
    } else if (tk.type === "val") { postfix.push(tk.token) }
    else if (tk.token === "(") { op.push(tk) }
    else if (tk.token === ")") {
      while (op.length > 0 && op.peek().token !== "(") {
        postfix.push(op.pop().token)
      }
      if (op.length === 0) { error(`invalid expression ${infix}`) }
      op.pop()
    }
  }
  while (op.length > 0) {
    if (op.peek().type !== "op") { error(`invalid expression ${infix}`) }
    postfix.push(op.pop().token)
  }
  return postfix.join(" ")
}

// O(n) converts an infixt expression to a prefix expression
export function infixToPrefix(infix) {
  function construct() {
    if (val.length < 2) { error(`invalid expression ${infix}`) }
    const b = val.pop(), a = val.pop()
    val.push(`${op.pop().token}${a}${b}`)
  }
  const op = new Stack(), val = new Stack()
  for (const tk of tokenize(infix)) {
    if (tk.type === "op") {
      while (op.length > 0 && op.peek().prcd >= tk.prcd) { construct() }
      op.push(tk)
    } else if (tk.type === "val") { val.push(tk.token) }
    else if (tk.token === "(") { op.push(tk) }
    else if (tk.token === ")") {
      while (op.length > 0 && op.peek().token !== "(") { construct() }
      if (op.length === 0) { error(`invalid expression ${infix}`) }
      op.pop()
    }
  }
  while (op.length > 0) { construct() }
  if (val.length !== 1) { error(`invalid expression ${infix}`) }
  return val.pop().split("").join(" ")
}

// O(n) evaluates a postfix arithmetic expression
export function postfixEvaluate(postfix) {
  const op = new Stack()
  for (const { token, type } of tokenize(postfix)) {
    type === "op" ? evaluate(token, op) : op.push(token)
  }
  if (op.length !== 1) { error(`invalid expression ${postfix}`) }
  return op.pop()
}
