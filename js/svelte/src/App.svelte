<script>
  import Clock from "./Clock.svelte"
  import Loan from "./Loan.svelte"
  import TodoList from "./TodoList.svelte"
  import CssClass from "./CssClass.svelte"
  import Counter from "./Counter.svelte"
  import Select from "./Select.svelte"
  const options = [
    "", "Red", { label: "Green" }, { label: "Blue", value: "b" }
  ]
  let selected
  import Dog from "./Dog.svelte"
  import EmployeeList from "./EmployeeList.svelte"
  import StopLight from "./StopLight.svelte"
  import BindForm from "./BindForm.svelte"

  // Travel packing checklist
  import Login from "./Login.svelte"
  import CheckList from "./CheckList.svelte"
</script>

<!-- <Clock color="green"/> -->
<!-- <Loan amount="1000" interest="1" years="1"/> -->
<!-- <TodoList/> -->
<!-- <CssClass/> -->
<!-- <Counter counter="4"/> -->
<!-- <Select {options} on:select={e => selected = e.detail}/>
     {#if selected}<p>Selected = {selected}</p>{/if} -->
<!-- <Dog breed="whippet"/> -->
<!-- <EmployeeList/> -->
<!-- <StopLight/> -->
<BindForm/>

<!-- Travel packing checklist -->
<!-- <main>
     <h1>Travel packing checklist</h1>
     <Login/>
     </main>

     <style>
     :global(body) {
     background-color: cornsilk;
     }
     main {
     display: flex;
     flex-direction: column;
     justify-content: flex-start;
     align-items: center;
     }
     h1 {
     text-align: center;
     }
     </style> -->
<!-- <CheckList/> -->
