<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()
  import { dogStore } from "./stores.js"

  $: dogs = Object.values($dogStore)
  let dogsSelected = []

  function dogStr(dog) {
    return `${dog.name} is a ${dog.size} ${dog.breed}`
  }
  function dogSelect(ev) {
    const { selectedOptions } = ev.target
    dogsSelected = Array.from(selectedOptions).map(option =>
      $dogStore[option.value]
    )
    dispatch("dogSelect", dogsSelected[0])
  }
  function dogDelete() {
    dogStore.update(dogs => {
      dogsSelected.map(dog => delete dogs[dog.id])
      return dogs
    })
    dogsSelected = []
  }
</script>

{#if dogs.length}
  <select multiple on:change={dogSelect}>
    {#each dogs as dog (dog.id)}
      <option value={dog.id}>{dogStr(dog)}</option>
    {/each}
  </select>
{:else}
  <p>Dog list is empty</p>
{/if}
<button type="button"
        on:click={() => dispatch("mode", "dogCreate")}>Add</button>
<button type="button" disabled={dogsSelected.length === 0}
              on:click={() => dispatch("mode", "dogUpdate")}>Edit</button>
<button type="button" disabled={dogsSelected.length === 0}
              on:click={dogDelete}>Remove</button>
