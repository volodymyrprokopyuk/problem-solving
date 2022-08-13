<script>
  let status = 200
  $: error = status >= 400
</script>

<input type="number" bind:value={status}>

<div class:error={status >= 400}>{status}</div>
<div class:error>{status}</div>
<div class={status >= 400 ? "error" : ""}>{status}</div>

<style>
  .error { color: red; }
</style>
