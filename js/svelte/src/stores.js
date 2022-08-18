import { readable, writable } from "svelte/store"

let counter = 0

export const incCounter = writable(counter, set => {
  // const token = setInterval(() => set(++counter), 1000)
  const token = setInterval(() => { counter += 1; set(counter) }, 1000)
  return () => clearInterval(token)
})
