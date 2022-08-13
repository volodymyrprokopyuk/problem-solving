<script>
  export let counter = 0
  function inc() { ++counter }
  let k = 1
  let double = 0
  $: {
    k = 2
    double = counter * k
  }
</script>

<p>Counter = {counter}. Double = {counter * 2}</p>
<p>Counter = {counter}. Double = {double}</p>
<button on:click={inc}>+</button>
<button on:click={() => --counter}>-</button>
