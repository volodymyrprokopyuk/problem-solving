<script>
  export let name = ""
  export let activePage = ""
</script>

<button type="button" class:active={activePage === name}
        on:click={() => activePage = name}>
  <slot/>
</button>

<style>
  .active {
    background-color: cornsilk;
  }
</style>
