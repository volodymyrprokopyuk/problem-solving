export function arrayEqual(a, b) {
  if (a.length !== b.length) {
    return false
  }
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) {
      return false
    }
  }
  return true
}

export function listenForEvent(contract, eventName, timeout = 1000) {
  return new Promise((resolve, reject) => {
    setTimeout(() => reject("event timeout"), timeout)
    contract.on(eventName, (...args) => {
      const event = args.at(-1)
      resolve(event)
    })
  })
}

export function toEmitWithArgs(event, ...args) {
  const a = event.log.args, b = args
  const pass = arrayEqual(a, b)
  const message = `expected ${a}${pass ? " not " : " "}to equal ${b}`
  return { pass, message }
}
