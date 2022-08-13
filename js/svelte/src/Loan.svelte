<script>
 export let amount = 1000
 export let interest = 1
 export let years = 1
 $: minterest = interest / 12 / 100
 $: months = years * 12
 $: mquote = amount * (1 + minterest) ** months / months
</script>

<label for="loan">Loan amount</label>
<input id="loan" type="number" bind:value={amount}>
<label for="interest">Interest rate</label>
<input id="interest" type="number" bind:value={interest}>
<label for="years">Years</label>
<input id="years" type="number" bind:value={years}>
<div class="quote">Monthly quote {mquote.toFixed(3)}</div>

<style>.quote { color: blue; }</style>
