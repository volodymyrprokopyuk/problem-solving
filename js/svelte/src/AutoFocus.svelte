<script>
  import { onMount } from "svelte"

  let nameInput
  let name

  onMount(() => nameInput.focus())

  function focus(el) { el.focus() }
</script>

<!-- <input type="text" placeholder="Your name"
     bind:this={nameInput} bind:value={name} use:focus> -->
<input type="text" placeholder="Your name"
       bind:this={nameInput} bind:value={name}>
{#if name}<p>Name: {name}</p>{/if}
