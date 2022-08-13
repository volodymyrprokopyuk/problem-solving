<script>
  let employees = []

  async function getEmployees() {
    const url = "https://dummy.restapiexample.com/api/v1/employees"
    const res = await fetch(url)
    const { status, data } = await res.json()
    if (status === "success") { return data }
    else { throw new Error(`API error ${status}`) }
  }
</script>

{#await getEmployees()}
  <p>Loading employees...</p>
{:then emps}
  <table>
    <caption>Employees</caption>
    <tr><th>Name</th><th>Age</th></tr>
    {#each emps as { employee_name: name, employee_age: age }}
      <tr><td>{name}</td><td><td>{age}</td></tr>
    {/each}
  </table>
{:catch e}
  <p class="error">{e}</p>
{/await}
