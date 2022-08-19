<script>
  import { taxStore, taxedItemStore } from "./stores.js"
</script>

<label>Tax <input type="number" bind:value={$taxStore}></label>
{#each $taxedItemStore as item}
  <p>{item.name}, cost: {item.cost}, total: {item.total}</p>
{/each}
