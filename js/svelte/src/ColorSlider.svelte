<script>
  export let name
  export let color
</script>

<label>{name} <input type="range" min="0" max="255" bind:value={color}></label>
