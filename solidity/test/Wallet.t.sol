// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {Wallet} from "contract/Wallet.sol";

contract WalletTest is Test {
  Wallet wallet;
  address payable owner;

  function setUp() public {
    owner = payable(makeAddr("owner"));
    vm.prank(owner);
    wallet = new Wallet();
  }

  function testSendWithdraw() public {
    // Deposit and balance
    uint value1 = 2 ether;
    (bool success, ) = address(wallet).call{value: value1}("");
    require(success, "send to walled failed");
    assertEq(wallet.balance(), value1);
    // Withdraw 10 ether => ErrInsufficientFunds
    uint value2 = 10 ether;
    bytes memory err = abi.encodeWithSelector(
      Wallet.ErrInsufficientFunds.selector, value2
    );
    vm.expectRevert(err);
    vm.prank(owner);
    wallet.withdraw(value2);
    // Withdraw 1 ether
    uint value3 = 1 ether;
    vm.prank(owner);
    wallet.withdraw(value3);
    assertEq(owner.balance, value3);
    assertEq(wallet.balance(), value1 - value3);
  }
}
