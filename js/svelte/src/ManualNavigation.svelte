<script>
  import NavButton from "./NavButton.svelte"
  import NavShop from "./NavShop.svelte"
  import NavCart from "./NavCart.svelte"
  import NavShip from "./NavShip.svelte"

  const pageMap = { shop: NavShop, cart: NavCart, ship: NavShip }
  let activePage = "shop"
</script>

<nav>
  <NavButton name="shop" bind:activePage>Shop</NavButton>
  <NavButton name="cart" bind:activePage>Cart</NavButton>
  <NavButton name="ship" bind:activePage>Ship</NavButton>
</nav>
<main>
  <svelte:component this={pageMap[activePage]}/>
</main>
