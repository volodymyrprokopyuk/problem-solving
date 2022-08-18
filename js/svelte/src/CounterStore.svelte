<!-- <script>
     import { onDestroy } from "svelte"
     import { incCounter } from "./stores"

     let counter = 0
     const unsubscribe = incCounter.subscribe(newVal => counter = newVal)
     onDestroy(unsubscribe)
     </script>

     <p>Counter = {counter}</p>
     <button type="button" on:click={() => incCounter.set(0)}>Reset</button> -->

<script>
 import { incCounter } from "./stores"
</script>

<p>Counter = {$incCounter}</p>
<button type="button" on:click={() => $incCounter = 0}>Reset</button>
