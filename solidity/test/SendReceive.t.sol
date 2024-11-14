// SPDX-License-Identifier: GPL-3.0-or-later
pragma solidity ^0.8.0;

import {Test, console} from "forge-std/Test.sol";

import {Send, Receive} from "contract/SendReceive.sol";

contract SendReceiveTest is Test {
  Send send;
  Receive recv;

  function setUp() public {
    send = new Send();
    recv = new Receive();
  }

  function testTransfer() public {
    (address from, address to) = (address(send), address(recv));
    hoax(from, 2 ether);
    assertEq(from.balance, 2 ether);
    assertEq(to.balance, 0 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
    // indexed topic1, topic2, topic3, non-indexed data
    vm.expectEmit(true, false, false, true);
    emit Receive.EvReceive(from, 1 ether); // expected event
    send.sendTransfer{value: 1 ether}(payable(to)); // actual event
    assertEq(from.balance, 1 ether);
    assertEq(to.balance, 1 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
  }

  function testSend() public {
    (address from, address to) = (address(send), address(recv));
    hoax(from, 2 ether);
    assertEq(from.balance, 2 ether);
    assertEq(to.balance, 0 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
    vm.expectEmit(true, false, false, true);
    emit Receive.EvReceive(from, 1 ether);
    send.sendSend{value: 1 ether}(payable(to));
    assertEq(from.balance, 1 ether);
    assertEq(to.balance, 1 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
  }

  function testCall() public {
    (address from, address to) = (address(send), address(recv));
    hoax(from, 2 ether);
    assertEq(from.balance, 2 ether);
    assertEq(to.balance, 0 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
    vm.expectEmit(true, false, false, true);
    emit Receive.EvReceive(from, 1 ether);
    send.sendCall{value: 1 ether}(payable(to));
    assertEq(from.balance, 1 ether);
    assertEq(to.balance, 1 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
  }

  function testDeposit() public {
    (address from, address to) = (address(send), address(recv));
    hoax(from, 2 ether);
    assertEq(from.balance, 2 ether);
    assertEq(to.balance, 0 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
    vm.expectEmit(true, false, false, true);
    emit Receive.EvReceive(from, 1 ether);
    send.sendDeposit{value: 1 ether}(payable(to));
    assertEq(from.balance, 1 ether);
    assertEq(to.balance, 1 ether);
    console.log("=== Send balance: %s", from.balance);
    console.log("=== Receive balance: %s", to.balance);
  }
}
