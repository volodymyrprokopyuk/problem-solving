// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {Token} from "contract/Token.sol";

contract TokenTest is Test {
  Token token;
  uint supply = 1000 ether;

  function setUp() public {
    token = new Token(supply);
  }

  function testTransferSuccess() public {
    (address from, address to, uint value) =
      (address(this), makeAddr("to"), 1 ether);
    assertEq(token.balances(from), supply);
    vm.expectEmit(true, true, false, true);
    emit Token.EvTransfer(from, to, value);
    token.transfer{value: value}(to);
    assertEq(token.balances(from), supply - value);
    assertEq(token.balances(to), value);
  }

  function testTransferErrInsufficientFunds() public {
    (address from, address to, uint value) =
      (address(this), makeAddr("to"), supply + 1 ether);
    bytes memory err =
      abi.encodeWithSelector(Token.ErrInsufficientFunds.selector, value);
    vm.expectRevert(err);
    token.transfer{value: value}(to);
    assertEq(token.balances(from), supply);
    assertEq(token.balances(to), 0);
  }
}
