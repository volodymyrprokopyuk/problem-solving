<script>
  let clName
  export { clName as class }
  export let bgColor
</script>

<div class={clName} style:background-color={bgColor}>Component</div>

<style>
 .cmp {
   color: var(--color, yellow);
 }
</style>
