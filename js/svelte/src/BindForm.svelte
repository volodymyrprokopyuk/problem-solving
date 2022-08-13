<script>
  const flavors = ["Vanilla", "Chocolate", "Strawberry"]
  const seasons = ["Spring", "Summer", "Fall", "Winter"]
  const colors = ["Red", "Orange", "Yellow", "Blue", "Green", "Purple"]
  let name = "Vlad"
  let happy = true
  let favFlavors = []
  let favSeason = ""
  let favColors = []
  let story = ""
</script>

<div class="form">
  <label>Name <input type="text" placeholder="Your name"
                     bind:value={name}></label>
  <label>Happy? <input type="checkbox" bind:checked={happy}></label>
  <label>Favorite flavors</label>
  {#each flavors as flavor}
    <label><input type="checkbox" value={flavor}
                  bind:group={favFlavors}>{flavor}</label>
  {/each}
  <label>Favorite season</label>
  {#each seasons as season}
    <label><input type="radio" value={season}
                  bind:group={favSeason}>{season}</label>
  {/each}
  <label>Favorite color
    <select multiple bind:value={favColors}>
      <option/>
      {#each colors as color}
        <option>{color}</option>
      {/each}
    </select>
  </label>
  <label>Life story <textarea bind:value={story}></textarea>
</div>

{#if name}
  <p>{name} is {happy ? "happy" : "unhappy"}</p>
  <p>Favorite flavors: {favFlavors.join(", ")}</p>
  <p>Favorite season: {favSeason}</p>
  <p>Favorite color: {favColors.join(", ")}</p>
  <p>Life story: {story}</p>
{/if}
