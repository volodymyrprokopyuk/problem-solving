<script>
  import ColorSlider from "./ColorSlider.svelte"

  let red = 0
  let green = 0
  let blue = 0

  $: hex = "#" + toHex(red) + toHex(green) + toHex(blue)

  function toHex(value) {
    const hex = value.toString(16)
    return hex.length === 1 ? "0" + hex : hex
  }
</script>

<ColorSlider name="Red" bind:color={red}/>
<ColorSlider name="Green" bind:color={green}/>
<ColorSlider name="Blue" bind:color={blue}/>
<div class="swatch" style="background-color: {hex};">{hex}</div>

<style>
  .swatch {
    width: 7em;
    height: 7em;
    display: flex;
    justify-content: center;
    align-items: center;
  }
</style>
