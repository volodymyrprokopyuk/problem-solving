<script>
  let markup = '<h1 style="color: blue;">Hello</h1>'
</script>

<textarea bind:value={markup}/>
<div>Preview: {@html markup}</div>
