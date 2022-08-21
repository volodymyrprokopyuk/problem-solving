<script>
  import { onMount, beforeUpdate, afterUpdate, onDestroy } from "svelte"

  const colors = ["red", "green", "blue", "yellow", "magenta", "cayan"]
  let cIndex = 0
  let color = colors[cIndex]

  function colorChange() {
    color = colors[++cIndex % colors.length]
  }

  onMount(() => console.log("onMount"))
  beforeUpdate(() => console.log("beforeUpdate"))
  afterUpdate(() => console.log("afterUpdate"))
  onDestroy(() => console.log("onDestroy"))
</script>

<button type="button" on:click={colorChange} style="color: {color}">
  Change</button>
