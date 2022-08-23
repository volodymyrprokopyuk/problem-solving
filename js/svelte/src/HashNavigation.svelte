<script>
  import NavShop from "./NavShop.svelte"
  import NavCart from "./NavCart.svelte"
  import NavShip from "./NavShip.svelte"

  const hashMap = { "#shop": NavShop, "#cart": NavCart, "#ship": NavShip }
  let activeComponent = hashMap["#shop"]
</script>

<svelte:window on:hashchange={() => activeComponent = hashMap[location.hash]}/>
<nav>
  <a href="/#shop" class:active={activeComponent === NavShop}>Shop</a>
  <a href="/#cart" class:active={activeComponent === NavCart}>Cart</a>
  <a href="/#ship" class:active={activeComponent === NavShip}>Ship</a>
</nav>
<main>
  <svelte:component this={activeComponent}/>
</main>

<style>
  .active {
    text-decoration: underline;
    font-weight: bold;
  }
</style>
