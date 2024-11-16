// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {PaymentClaim} from "contract/PaymentClaim.sol";

contract PaymentClaimTest is Test {
  PaymentClaim pc;
  address payable owner;
  uint ownerKey;
  uint fund = 10 ether;
  address payable claimer;

  function setUp() public {
    (address addr, uint key) = makeAddrAndKey("owner");
    (owner, ownerKey) = (payable(addr), key);
    hoax(owner, fund);
    pc = new PaymentClaim{value: fund}();
    assertEq(owner.balance, 0);
    assertEq(address(pc).balance, fund);
    claimer = payable(makeAddr("claimer"));
  }

  function signClaim(address aClaimer, uint value, uint anOwnerKey)
    internal view returns (bytes memory) {
    uint nonce = uint(keccak256(abi.encode(block.timestamp)));
    bytes32 hash = keccak256(abi.encode(aClaimer, value, nonce));
    (uint8 v, bytes32 r, bytes32 s) = vm.sign(anOwnerKey, hash);
    bytes memory claim = abi.encode(nonce, v, r, s);
    return claim;
  }

  function testClaimPaymentSuccess() public {
    uint value = 1 ether;
    bytes memory claim = signClaim(claimer, value, ownerKey);
    vm.expectEmit(true, false, false, true);
    emit PaymentClaim.EvPayment(claimer, value);
    vm.prank(claimer);
    pc.claimPayment(value, claim);
    assertEq(claimer.balance, value);
    assertEq(address(pc).balance, fund - value);
    vm.prank(owner);
    pc.deactivate();
    assertEq(owner.balance, fund - value);
    assertEq(address(pc).balance, 0);
  }

  function testInvalidClaim() public {
    uint value = 1 ether;
    bytes memory claim = signClaim(claimer, value, ownerKey);
    vm.expectPartialRevert(PaymentClaim.ErrInvalidClaim.selector);
    vm.prank(claimer);
    pc.claimPayment(value + 1 ether, claim);
  }
}
