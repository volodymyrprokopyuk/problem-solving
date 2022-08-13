<script>
  import TodoItem from "./TodoItem.svelte"

  let lastId = 0
  let todoText = ""
  let todos = [
    todoCreate("Learn Svelte", true),
    todoCreate("Build Svelte app")
  ]

  $: status = `${todos.filter(t => t.done).length} of ${todos.length} done`

  function todoCreate(text, done = false) {
    return { id: ++lastId, text, done }
  }

  function todoDelete(id) {
    todos = todos.filter(t => t.id !== id)
  }

  function todoAdd() {
    todos = [...todos, todoCreate(todoText)]
    todoText = ""
  }

  function doneArchive() {
    todos = todos.filter(t => !t.done)
  }
</script>

<div>
  <h1>Todo list</h1>
  <span>Status: {status}</span>
  <button on:click={doneArchive}>Archive done</button>

  <form on:submit|preventDefault>
    <input type="text" placeholder="New todo" bind:value={todoText}>
    <button on:click={todoAdd} disabled={!todoText}>Add</button>
  </form>

  <ul>
    {#each todos as todo}
      <TodoItem {todo}
                on:todoToggle={() => todo.done = !todo.done}
                on:todoDelete={() => todoDelete(todo.id)}/>
    {/each}
  </ul>
</div>

<style>
  ul {
    list-style: none;
  }
</style>
