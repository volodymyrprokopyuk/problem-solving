// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {SafePurchase} from "contract/SafePurchase.sol";

contract SafePurchaseTest is Test {
  SafePurchase sp;
  address payable seller;
  address payable buyer;
  uint price = 10 ether;

  function setUp() public {
    seller = payable(makeAddr("seller"));
    buyer = payable(makeAddr("buyer"));
    // offer
    vm.expectEmit(true, false, false, true);
    emit SafePurchase.EvOffer(price * 2);
    hoax(seller, price * 2);
    sp = new SafePurchase{value: price * 2}();
    assertEq(seller.balance, 0);
    assertEq(address(sp).balance, price * 2);
  }

  function testPurchaseReceiveRefund() public {
    // purchase
    vm.expectEmit(true, false, false, true);
    emit SafePurchase.EvPurchase(price * 2);
    hoax(buyer, price * 2);
    sp.purchase{value: price * 2}();
    assertEq(buyer.balance, 0);
    assertEq(address(sp).balance, price * 4);
    // receive
    vm.expectEmit(true, false, false, true);
    emit SafePurchase.EvReceive(price);
    vm.prank(buyer);
    sp.received();
    assertEq(buyer.balance, price);
    assertEq(address(sp).balance, price * 3);
    // refund
    vm.expectEmit(true, false, false, true);
    emit SafePurchase.EvRefund(price * 3);
    vm.prank(seller);
    sp.refund();
    assertEq(seller.balance, price * 3);
    assertEq(address(sp).balance, 0);
  }

  function testCancelPurchaseError() public {
    // cancel
    vm.expectEmit(true, false, false, true);
    emit SafePurchase.EvCancel(price * 2);
    vm.prank(seller);
    sp.cancel();
    assertEq(seller.balance, price * 2);
    assertEq(address(sp).balance, 0);
    // purchase
    bytes memory err = abi.encodeWithSelector(
      SafePurchase.ErrInvalidState.selector, SafePurchase.State.Canceled
    );
    vm.expectRevert(err);
    hoax(buyer, price * 2);
    sp.purchase{value: price * 2}();
    assertEq(buyer.balance, price * 2);
  }
}
