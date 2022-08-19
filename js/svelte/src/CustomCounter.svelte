<script>
  import { customCounter } from "./stores.js"
</script>

<p>Count = {$customCounter}</p>
<button type="button" on:click={() => customCounter.inc()}>+</button>
<button type="button" on:click={() => customCounter.dec()}>-</button>
<button type="button" on:click={() => customCounter.reset()}>Reset</button>
