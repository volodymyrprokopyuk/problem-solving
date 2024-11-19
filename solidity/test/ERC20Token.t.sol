// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {IERC20, ERC20Token} from "contract/ERC20Token.sol";

contract ERC20TokenTest is Test {
  address payable owner;
  address payable spender;
  address payable acc1;
  uint totalSupply = 10 ether;
  IERC20 token;

  function setUp() public {
    (owner, spender, acc1) =
      (payable(makeAddr("owner")),
       payable(makeAddr("spender")),
       payable(makeAddr("acc1"))
      );
    token = new ERC20Token(owner, totalSupply);
    assertEq(token.balanceOf(owner), totalSupply);
  }

  function testTransfer() public {
    // Transfer success
    uint value = 1 ether;
    vm.expectEmit(true, true, false, true);
    emit IERC20.Transfer(owner, acc1, value);
    vm.prank(owner);
    token.transfer(acc1, value);
    assertEq(token.balanceOf(acc1), value);
    assertEq(token.balanceOf(owner), totalSupply - value);
    // Transfer => ErrInsufficientFunds
    bytes memory err = abi.encodeWithSelector(
      ERC20Token.ErrInsufficientFunds.selector, acc1, value * 2
    );
    vm.expectRevert(err);
    vm.prank(acc1);
    token.transfer(owner, value * 2);
  }

  function testApproveTransferFrom() public {
    // Approve
    uint value = 2 ether;
    vm.expectEmit(true, true, false, true);
    emit IERC20.Approval(owner, spender, value);
    vm.prank(owner);
    token.approve(spender, value);
    assertEq(token.allowance(owner, spender), value);
    // Transfer from success
    vm.expectEmit(true, true, false, true);
    emit IERC20.Transfer(owner, acc1, value);
    vm.prank(spender);
    token.transferFrom(owner, acc1, value);
    assertEq(token.balanceOf(acc1), value);
    assertEq(token.balanceOf(owner), totalSupply - value);
    assertEq(token.allowance(owner, spender), 0);
    // Transfer from => ErrBeyondAllowance
    bytes memory err = abi.encodeWithSelector(
      ERC20Token.ErrBeyondAllowance.selector, owner, spender, value
    );
    vm.expectRevert(err);
    vm.prank(spender);
    token.transferFrom(owner, acc1, value);
  }
}
