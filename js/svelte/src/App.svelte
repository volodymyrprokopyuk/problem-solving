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
  import Tally from "./Tally.svelte"
  let tally
  let rate = 0.0
  let grandTotal = 0.0
  function tallyExtract() {
    rate = tally.rate
    grandTotal = tally.grandTotal()
  }
  import BindChild from "./BindChild.svelte"
  let pValue = 1
  let triple
  import ColorPicker from "./ColorPicker.svelte"
  import SlotShipping from "./SlotShipping.svelte"
  import EventButtons from "./EventButtons.svelte"
  const buttons = ["A", "B", "C"]
  let bselected
  function buttonSelected(ev) { bselected = ev.detail }
  import CounterStore from "./CounterStore.svelte"
  import DogList from "./DogList.svelte"
  import DogForm from "./DogForm.svelte"
  let dog = { }
  let mode = "dogList"
  function modeChange(ev) {
    mode = ev.detail
    if (mode === "dogCreate") { dog = { } }
  }
  import DerivedCost from "./DerivedCost.svelte"
  import CustomCounter from "./CustomCounter.svelte"

  // Travel packing checklist
  import Login from "./Login.svelte"
  import CheckList from "./CheckList.svelte"
  let page = Login
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
<!-- <BindForm/> -->
<!-- <Tally bind:this={tally}/>
     <button type="button" on:click={tallyExtract}>Extract</button>
     <p>Rate: {rate.toFixed(2)}, Grand total: {grandTotal.toFixed(2)}</p> -->
<!-- <BindChild bind:cValue={pValue} bind:triple/>
     <p>pValue = {pValue}, triple = {triple}</p> -->
<!-- <ColorPicker/> -->
<!-- <SlotShipping>
     <p slot="address">My address</p>
     <p slot="name">Vlad</p>
     </SlotShipping> -->
<!-- <EventButtons {buttons} selected={bselected} on:buttonSelected={buttonSelected}/>
     {#if bselected}<p>Selected: {bselected}</p>{/if} -->
<!-- <CounterStore/> -->
<!-- <h1>Dogs</h1>
     {#if mode === "dogList"}
     <DogList on:mode={modeChange} on:dogSelect={ev => dog = ev.detail}/>
     {:else}
     <DogForm {mode} {dog} on:mode={modeChange}/>
     {/if} -->
<!-- <DerivedCost/> -->
<CustomCounter/>

<!-- Travel packing checklist -->
<!-- <main>
     <h1>Travel packing checklist</h1>
     {#if page === Login}
     <Login on:login={() => page = CheckList}/>
     {:else}
     <CheckList on:logout={() => page = Login}/>
     {/if}
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
