<script>
  import Item from "./Item.svelte"
  import { blurOnKey } from "./util.js"

  export let categories = []
  export let category = { id: 0, name: "", items: [] }
  export let show = "all"

  let editing = false
  let itemName = ""

  $: status = `${category.items.filter(it => !it.packed).length} of ${items.length} unpacked`
  $: items = category.items.filter(it => show === "all"
                         || show === "packed" && it.packed
                         || show === "unpacked" && !it.packed)

  function itemAdd() {
    const itemNames = new Set(categories.flatMap(cat =>
      cat.items.map(it => it.name))
    )
    if (itemNames.has(itemName)) {
      alert(`${itemName} already exists`)
      return
    }
    const id = Math.floor(Math.random() * 1000)
    category.items.push({ id, name: itemName, packed: false })
    category.items = category.items
    itemName = ""
  }
</script>

<section>
  <h3>
    {#if editing}
      <input type="text" bind:value={category.name}
        on:blur={() => editing = false}
        on:keypress={blurOnKey}>
    {:else}
      <span on:click={() => editing = true}>{category.name}</span>
    {/if}
    <button>Delete</button>
  </h3>
  <p>{status}</p>

  <form on:submit|preventDefault={itemAdd}>
    <input type="text" placeholder="New item" bind:value={itemName}>
    <button type="submit" disabled={!itemName}>Add item</button>
  </form>

  <ul>
    {#each items as item (item.id)}
      <Item bind:item/>
    {:else}
      <p>Empty category</p>
    {/each}
  </ul>
</section>

<style>
  ul { list-style: none; }
</style>
