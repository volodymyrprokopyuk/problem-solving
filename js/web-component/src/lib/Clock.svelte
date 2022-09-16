<svelte:options tag="sv-clock"/>

<script>
  import { onMount, onDestroy } from "svelte"
  let dt = new Date()
  let tk
  onMount(() => tk = setInterval(() => dt = new Date(), 1000))
  onDestroy(() => clearInterval(tk))
</script>

<div>{dt}</div>
