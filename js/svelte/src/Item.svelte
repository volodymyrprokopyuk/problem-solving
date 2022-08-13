<script>
  import { blurOnKey } from "./util.js"

  export let item = { id: 0, name: "", packed: false }

  let editing = false
</script>

<li>
  <input type="checkbox" bind:checked={item.packed}>
  {#if editing}
    <input type="text" autofocus
           bind:value={item.name}
           on:blur={() => editing = false}
           on:keydown={blurOnKey}>
  {:else}
    <span class:packed={item.packed}
          on:click={() => editing = true}>{item.name}</span>
  {/if}
  <button>Delete</button>
</li>

<style>
  .packed {
    color: gray;
    text-decoration: line-through;
  }
</style>
