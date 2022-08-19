import { readable, writable } from "svelte/store"

let counter = 0

export const incCounter = writable(counter, set => {
  // const token = setInterval(() => set(++counter), 1000)
  const token = setInterval(() => { counter += 1; set(counter) }, 1000)
  return () => clearInterval(token)
})

export const dogStore = writable({
  1: {id: 1, name: "A", size: "small", breed: "B"},
  2: {id: 2, name: "C", size: "small", breed: "D"}
})
