<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()

  export let options

  function getLabel(option) {
    return typeof option === "object" ? option.label : option
  }

  function getValue(option) {
    return typeof option === "object" ? option.value || option.label : option
  }
</script>

<select on:change={e => dispatch("select", e.target.value)}>
  {#each options as option}
    <option value={getValue(option)}>{getLabel(option)}</option>
  {/each}
</select>
