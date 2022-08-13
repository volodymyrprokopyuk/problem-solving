<script>
  export const rate = 0.07
  export function grandTotal() { return total * (1 + rate) }

  let price = ""
  let prices = []

  $: total = prices.reduce((s, p) => s + p, 0.0)

  function priceAdd() {
    prices.push(price)
    prices = prices
    price = ""
  }
</script>

<input type="number" placeholder="New price" bind:value={price}>
<button type="button" on:click={priceAdd}>Add price</button><br/>
{#each prices as price}
  <span>{price}</span>
{/each}
<p>Total: {total.toFixed(2)}, Grand total: {(total * (1 + rate)).toFixed(2)}</p>

<style>
 span {
   margin-right: 1em;
 }
</style>
