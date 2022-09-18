<svelte:options tag="sv-slider"/>

<script>
</script>

<div class="slider">
  <div class="bg"></div>
  <div class="thumb"></div>
</div>

<style>
  .slider {
    display: inline-block;
    position: relative;
    width: 20em;
    height: 2em;
  }
  .bg {
    position: absolute;
    width: 100%;
    height: 100%;
    background-color: red;
  }
  .thumb {
    position: absolute;
    width: 0.5em;
    height: calc(100% - 0.2em);
    margin-left: 10em;
    border: 0.1em solid white;
    border-radius: 0.2em;
  }
</style>
