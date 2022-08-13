<script>
  export let color = "black"
  let time = new Date().toLocaleTimeString()
  setInterval(() => time = new Date().toLocaleTimeString(), 1000)
</script>

<span style="color: {color};">{time}</span>
