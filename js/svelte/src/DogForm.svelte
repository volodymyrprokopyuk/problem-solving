<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()
  import { dogStore } from "./stores.js"

  export let dog = { }
  export let mode = "dogCreate"

  let { name, size, breed } = dog

  $: dogValid = name && size && breed

  function dogSave() {
    const id = dog.id || Math.floor(Math.random() * 1000)
    dogStore.update(dogs => {
      dogs[id] = { id, name, size, breed }
      return dogs
    })
    dispatch("mode", "dogList")
  }
</script>

<form on:submit|preventDefault={dogSave}>
  <label>Name <input type="text" bind:value={name}></label>
  <label>Size</label>
  <label><input type="radio" value="small" bind:group={size}>Small</label>
  <label><input type="radio" value="medium" bind:group={size}>Medium</label>
  <label><input type="radio" value="large" bind:group={size}>Large</label>
  <label>Breed <input type="text" bind:value={breed}></label>
  <button type="submit" disabled={!dogValid}>
    {mode === "dogCreate" ? "Save" : "Update"}
  </button>
  <button type="button"
          on:click={() => dispatch("mode", "dogList")}>Cancel</button>
</form>
