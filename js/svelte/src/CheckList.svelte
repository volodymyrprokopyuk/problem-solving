<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()
  import Category from "./Category.svelte"

  let catName = ""
  let show = "all"
  let categories

  catsRestore()

  function catAdd() {
    const catNames = new Set(categories.map(cat => cat.name))
    if (catNames.has(catName)) {
      alert(`${catName} already exists`)
      return
    }
    const id = Math.floor(Math.random() * 1000)
    categories.push({ id, name: catName, items: [] })
    categories = categories
    catName = ""
    dispatch("persist")
  }

  function catDelete(id) {
    categories = categories.filter(cat => cat.id !== id)
    dispatch("persist")
  }

  function catsPersist() {
    localStorage.setItem("travelPacking", JSON.stringify(categories))
  }

  function catsRestore() {
    const catsDefault = [{
      id: 1, name: "Clothes", items: [
        {id: 1, name: "Item A", packed: false},
        {id: 2, name: "Item B", packed: true}
      ]
    }, {
      id: 2, name: "Bath", items: [
        {id: 3, name: "Item C", packed: true},
        {id: 4, name: "Item D", packed: false}
      ]
    }
    ]
    categories = JSON.parse(localStorage.getItem("travelPacking"))
    if (!categories) { categories = catsDefault }
  }

  function allUnpack() {
    categories.forEach(cat => cat.items.forEach(it => it.packed = false))
    categories = categories
  }
</script>

<svelte:head>
  <title>CheckList</title>
</svelte:head>
<section on:persist={catsPersist}>
  <div>
    <button on:click={() => dispatch("logout")}>Logout</button>
  </div>
  <form on:submit|preventDefault={catAdd}>
    <input type="text" placeholder="New category"
           bind:value={catName}>
    <button type="submit" disabled={!catName}>Add category</button>
  </form>
  <div class="show">
    <label>Show</label>
    <label>
      <input type="radio" name="show" value="all" bind:group={show}>
      All
    </label>
    <label>
      <input type="radio" name="show" value="packed" bind:group={show}>
      Packed
    </label>
    <label>
      <input type="radio" name="show" value="unpacked" bind:group={show}>
      Unpacked
    </label>
    <button type="button" on:click={allUnpack}>Unpack all</button>
  </div>
  <div class="categories">
    {#each categories as category (category.id)}
      <Category {categories} bind:category {show}
                on:delete={(ev) => catDelete(ev.detail)}
                on:persist={catsPersist}/>
    {/each}
  </div>
</section>

<style>
  .show {
    display: flex;
    align-items: center;
  }
  .show label {
    margin-left: 1em;
  }
  .show button {
    margin-left: 1em;
  }
</style>
