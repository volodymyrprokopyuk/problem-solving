// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Counter} from "contract/Counter.sol";

contract CounterTest is Test {
  Counter public counter;

  constructor() {
    console.log("=> before all");
  }

  function setUp() public {
    console.log("=> before each");
    counter = new Counter();
    counter.setNumber(0);
  }

  function testIncrement() public {
    counter.increment();
    assertEq(counter.number(), 1);
  }

  function testFailDoRevert() public view {
    counter.doRevert();
  }

  function testFuzzSetNumber(uint256 x) public {
    counter.setNumber(x);
    assertEq(counter.number(), x);
  }
}

abstract contract Shared {
  uint256 public constant number = 1234;
}

contract MultiTxTest is Test, Shared {
  uint256 a;
  uint256 b;

  function beforeTestSetup(bytes4 testSelector) public pure
    returns (bytes[] memory) {
    if (testSelector == this.testC.selector) {
      bytes[] memory testCalldata = new bytes[](2);
      testCalldata[0] = abi.encodePacked(this.testA.selector);
      testCalldata[1] = abi.encodeWithSignature("setB(uint256)", number);
      return testCalldata;
    }
    return new bytes[](0);
  }

  function testA() public {
    require(a == 0);
    a = 1;
  }

  function setB(uint256 value) public {
    b = value;
  }

  function testC() public view {
    assertEq(a, 1);
    assertEq(b, number);
  }
}
