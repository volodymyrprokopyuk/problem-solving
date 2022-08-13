<script>
  let color = "red"

  function cycle() {
    switch (color) {
      case "red": color = "yellow"; break
      case "yellow": color = "green"; break
      case "green": color = "red"; break
      default: color = "red"
    }
  }
</script>

<div class="light red" class:hide={color !== "red"}>Red</div>
<div class="light yellow" class:hide={color !== "yellow"}>Yellow</div>
<div class="light green" class:hide={color !== "green"}>Green</div>
<button type="button" on:click={cycle}>Cycle</button>

<style>
  .hide {
    visibility: hidden;
  }
  .light {
    width: 6em;
    height: 6em;
    border-radius: 3em;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .red {
    color: white;
    background-color: red;
  }
  .yellow {
    color: black;
    background-color: yellow;
  }
  .green {
    color: white;
    background-color: green;
  }
</style>
