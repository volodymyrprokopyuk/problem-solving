<script>
  export let breed = "whippet"
 let dogs = getDogs()

  async function getDogs() {
    const url = `https://dog.ceo/api/breed/${breed.toLowerCase()}/images/random/2`
    const res = await fetch(url)
    if (!res.ok) { return [] }
    const { message } = await res.json()
    return message
  }
</script>

<label>Breed <input type="text" bind:value={breed}></label>
<button type="button" on:click={() => dogs = getDogs()}>Images</button>
{#await dogs}
  <p>Waiting for dogs...</p>
{:then urls}
  {#each urls as url}
    <img src={url} alt="dog">
  {:else}
    <p>No images</p>
  {/each}
{:catch e}
  <p class="error">Error {e.message}</p>
{/await}

<style>
  button {
    display: block;
  }
</style>
