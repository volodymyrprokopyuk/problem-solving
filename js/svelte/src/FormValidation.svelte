<script>
 const countries = ["US", "Canada"]

 let pCodeType = countries[0]
 let name = "Vald"
 let age = 38
 let email = "a@b"
 let url = "a://b"
 let pCode = ""

 const pCodeConfig = {
   "US": { re: /^\d{5}(?:-\d{4})?$/, ex: "12345-1234" },
   "Canada": { re: /^[A-Z]\d[A-Z] \d[A-Z]\d$/, ex: "A1B 2C3" }
 }

 function submit() { console.log("submit") }
</script>

<form on:submit|preventDefault={submit}>
  <fieldset>
    <legend>Country</legend>
    {#each countries as country}
      <label>
        <input type="radio" name="pCodeType" value={country}
               bind:group={pCodeType}>
        {country}
      </label>
    {/each}
  </fieldset>
  <label>Name
    <input type="text" required minlength="2" maxlength="40"
           placeholder="Your name" bind:value={name}></label>
  <label>Age
    <input type="number" required min="18" max="120" bind:value={age}></label>
  <label>Email
    <input type="email" required placeholder="Your email"
           bind:value={email}></label>
  <label>URL
    <input type="url" required placeholder="Your URL" bind:value={url}></label>
  <label>Postal code
    <input type="text" required placeholder="Your postal code"
           pattern={pCodeConfig[pCodeType].re.source}
           title={pCodeConfig[pCodeType].ex}
           bind:value={pCode}></label>
  <button type="submit">Submit</button>
</form>
<p>{pCodeType} {name} {age} {email} {url} {pCode}</p>

<style>
 :invalid:not(:placeholder-shown) {
   border-color: red;
 }
</style>
