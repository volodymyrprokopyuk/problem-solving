<script>
  import { onMount } from "svelte"

  const colors = ["red", "green", "blue", "yellow", "cayan", "magenta"]
  let cIndex = 0
  let show = true

  onMount(() => {
    const token = setInterval(() => cIndex = ++cIndex % colors.length, 1000)
    return () => { clearInterval(token); console.log("interaval cleared") }
  })
</script>

<label>Show <input type="checkbox" bind:checked={show}></label>
{#if show}
  <div style="background-color: {colors[cIndex]};"></div>
{/if}

<style>
  div {
    width: 4em;
    height: 3em;
  }
</style>
