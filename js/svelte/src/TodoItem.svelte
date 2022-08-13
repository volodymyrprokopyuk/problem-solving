<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()

  export let todo
</script>

<li>
  <input type="checkbox" checked={todo.done}
               on:change={() => dispatch("todoToggle")}>
  <span class={todo.done ? "done" : "todo"}>{todo.text}</span>
  <button on:click={() => dispatch("todoDelete")}>Delete</button>
</li>

<style>
  .done {
    text-decoration: line-through;
    color: gray;
  }
</style>
