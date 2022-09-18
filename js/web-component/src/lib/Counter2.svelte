<svelte:options tag="sv-counter"/>

<script>
 let cnt = 0
 function inc() { ++cnt }
</script>

<button type="button" on:click={inc}>
  Counter {cnt}
</button>
