<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()

  export let buttons = []
  export let selected
</script>

{#each buttons as button}
  <button type="button" class:selected={selected === button}
          on:click={() => dispatch("buttonSelected", button)}>{button}</button>
{:else}
  <p>No buttons</p>
{/each}

<style>
  .selected {
    background-color: cornsilk;
  }
</style>
