// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {ValueStore, ReentrancyAttack} from "contract/Reentrancy.sol";

contract ValueStoreTest is Test {
  ValueStore store;
  uint storeValue = 2 ether;
  ReentrancyAttack attack;
  uint attackValue = 1 ether;

  function setUp() public {
    // Set up store
    store = new ValueStore();
    vm.expectEmit(true, false, false, true);
    emit ValueStore.EvDeposit(address(this), storeValue);
    (bool success, ) = address(store).call{value: storeValue}("");
    assertTrue(success);
    assertEq(address(store).balance, storeValue);
    // Set up attack
    attack = new ReentrancyAttack{value: attackValue}(payable(address(store)));
    assertEq(address(attack).balance, attackValue);
  }

  // function testReentrantWithdraw() public {
  //   address attacker = address(attack);
  //   vm.expectEmit(true, false, false, true);
  //   emit ValueStore.EvWithdraw(attacker, attackValue);
  //   vm.expectEmit(true, false, false, true);
  //   emit ValueStore.EvWithdraw(attacker, attackValue);
  //   vm.expectEmit(true, false, false, true);
  //   emit ValueStore.EvWithdraw(attacker, attackValue);
  //   attack.attack(attackValue);
  //   assertEq(attacker.balance, attackValue + storeValue);
  // }

  function testNonReentrantWithdraw() public {
    address attacker = address(attack);
    bytes memory err = abi.encodeWithSelector(
      ValueStore.ErrWithdraw.selector, attacker, attackValue
    );
    vm.expectRevert(err);
    attack.attack(attackValue);
    assertEq(attacker.balance, attackValue);
    assertEq(address(store).balance, storeValue);
  }
}
