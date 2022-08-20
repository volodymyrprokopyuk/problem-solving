<script>
  export let title
  export let dialog
</script>

<dialog bind:this={dialog}>
  <header>{title}</header>
  <main>
    <button type="button" on:click={() => dialog.close()}>Close</button>
    <slot name="content"/>
  </main>
</dialog>
