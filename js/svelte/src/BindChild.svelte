<script>
  export let cValue = 0
  export let triple = 0

  $: triple = cValue * 3

  function double() { cValue *= 2 }
</script>

<p>cValue = {cValue}, triple = {triple}</p>
<button type="button" on:click={double}>Double</button>
