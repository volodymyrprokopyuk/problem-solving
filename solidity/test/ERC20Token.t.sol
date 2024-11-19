// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test} from "forge-std/Test.sol";
import {IERC20, ERC20Token, ERC20TokenSwap} from "contract/ERC20Token.sol";

contract ERC20TokenTest is Test {
  address owner;
  address spender;
  address acc1;
  uint totalSupply = 10;
  IERC20 token;

  function setUp() public {
    (owner, spender, acc1) =
      (makeAddr("owner"), makeAddr("spender"), makeAddr("acc1"));
    token = new ERC20Token(owner, totalSupply);
    assertEq(token.balanceOf(owner), totalSupply);
  }

  function testTransfer() public {
    // Transfer success
    uint value = 1;
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
    uint value = 2;
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

contract ERC20TokenSwapTest is Test {
  IERC20 tokenA;
  address ownerA;
  IERC20 tokenB;
  address ownerB;
  uint totalSupply = 10;
  ERC20TokenSwap swap;

  function setUp() public {
    (ownerA, ownerB) = (makeAddr("ownerA"), makeAddr("ownerB"));
    tokenA = new ERC20Token(ownerA, totalSupply);
    tokenB = new ERC20Token(ownerB, totalSupply);
    swap = new ERC20TokenSwap(address(tokenA), address(tokenB));
  }

  function testSwap() public {
    address swapper = address(swap);
    (uint valueA, uint valueB) = (1, 2);
    vm.prank(ownerA);
    bool success = tokenA.approve(swapper, valueA);
    assertTrue(success);
    assertEq(tokenA.allowance(ownerA, swapper), valueA);
    vm.prank(ownerB);
    success = tokenB.approve(swapper, valueB);
    assertTrue(success);
    assertEq(tokenB.allowance(ownerB, swapper), valueB);
    vm.prank(swapper);
    swap.swap(ownerA, valueA, ownerB, valueB);
    assertEq(tokenA.balanceOf(ownerA), totalSupply - valueA);
    assertEq(tokenA.balanceOf(ownerB), valueA);
    assertEq(tokenB.balanceOf(ownerB), totalSupply - valueB);
    assertEq(tokenB.balanceOf(ownerA), valueB);
  }
}
