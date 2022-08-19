import { readable, writable, derived } from "svelte/store"

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

export const itemStore = writable([
  { name: "A", cost: 1 }, { name: "B", cost: 2 }
])

export const taxStore = writable(0.10)

export const taxedItemStore = derived(
  [itemStore, taxStore], ([itemStore, taxStore]) =>
  itemStore.map(item => ({ ...item, total: item.cost * (1 + taxStore) }))
)

const { subscribe, set, update } = writable(0)
export const customCounter = {
  subscribe,
  inc() { update(c => c + 1) },
  dec() { update(c => c - 1) },
  reset() { set(0) }
}
