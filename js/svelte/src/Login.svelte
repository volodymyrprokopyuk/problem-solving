<script>
  import { createEventDispatcher } from "svelte"
  const dispatch = createEventDispatcher()

  let username = ""
  let password = ""

  function signup() { console.log("signup()") }
</script>

<svelte:head>
  <title>Login</title>
</svelte:head>
<section>
  <form on:submit|preventDefault={() => dispatch("login")}>
    <label>
      Username <input type="text" required bind:value={username}>
    </label>
    <label>
      Password <input type="password" required bind:value={password}>
    </label>
    <div class="buttons">
      <button type="submit">Login</button>
      <button type="button" on:click={signup}>Sign up</button>
    </div>
  </form>
</section>

<style>
  form {
    display: inline-block;
  }
  input {
    display: block;
  }
  label {
    display: block;
  }
  .buttons {
    display: flex;
    justify-content: space-between;
  }
</style>
